extern crate lazy_static;

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;
type Item<'input> = Spanned<Tok<'input>, (usize, usize), LexicalError>;

#[derive(Debug, Clone)]
pub enum Tok<'input> {
  Indent, Dedent, Newline, False, None, True, And, As, Assert, Async, Await,
  Break, Class, Continue, Def, Del, Elif, Else, Except, Finally, For, From,
  Global, If, Import, In, Is, Lambda, Nonlocal, Not, Or, Pass, Raise, Return,
  Try, While, With, Yield, Plus, Minus, Mul, Div, Mod, Lt, Le, Gt, Ge, Eq, Neq,
  Assign, LParen, RParen, LBracket, RBracket, Comma, Colon, Period, Arrow,
  Int(i32), String(&'input str), Id(&'input str)
}

#[derive(Debug)]
pub enum LexicalError {
  Unrecognized,
  IntegerOutOfRange,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum LexerState {
  Begin, InLine
}

pub struct Lexer<'input> {
  chars: &'input str,
  state: LexerState,
  indent_stack: Vec<isize>,
  line: usize,
  column: usize,
  pos: usize,
}

use regex::Regex;
use lazy_static::lazy_static;

macro_rules! re {
  ($id:ident, $v:literal) => { lazy_static! { static ref $id : Regex = Regex::new($v).unwrap(); } }
}

re!(ID,         r#"^[a-zA-Z_][a-zA-Z0-9_]*"#);
re!(INTEGER,    r#"^[0-9]+"#);
re!(STRING,     r#"^"(?:[\x20-\x21\x23-\x5b\x5d-\x7e]|\\n|\\t|\\\\|\\")*""#);
re!(COMMENT,    r#"^#[^\n|\r]*"#);
re!(WHITESPACE, r#"^(?: |\t)*"#);
re!(NEWLINE,    r#"^(?:\n|\r)+(?:(?: |\t)*(?:\n|\r))*"#);
re!(PLUS,       r#"^\+"#);
re!(MINUS,      r#"^-"#);
re!(MUL,        r#"^\*"#);
re!(DIV,        r#"^//"#);
re!(MOD,        r#"^%"#);
re!(LT,         r#"^<"#);
re!(LE,         r#"^<="#);
re!(GT,         r#"^>"#);
re!(GE,         r#"^>="#);
re!(EQ,         r#"^=="#);
re!(NEQ,        r#"^!="#);
re!(ASSIGN,     r#"^="#);
re!(LPAREN,     r#"^\("#);
re!(RPAREN,     r#"^\)"#);
re!(LBRACKET,   r#"^\["#);
re!(RBRACKET,   r#"^\]"#);
re!(COMMA,      r#"^,"#);
re!(COLON,      r#"^:"#);
re!(PERIOD,     r#"^\."#);
re!(ARROW,      r#"^->"#);

impl<'input> Lexer<'input> {
  /*
//  type Item = Spanned<Tok<'input>, (usize, usize), LexicalError>;
  type Loc = (usize, usize);
  type Error = LexicalError;
  type Item = Result<(Loc, Tok<'input>, Loc), Error>;
  */

  pub fn new(input: &'input str) -> Self {
    Lexer {
      chars: input,
      state: LexerState::Begin,
      indent_stack: vec![0],
      line: 1,
      column: 1,
      pos: 0,
    }
  }

  pub fn str_to_tok(word: &'input str) -> Tok<'input> {
    match word {
      "None" => Tok::None,

      "True" => Tok::True,
      "False" => Tok::False,

      "and" => Tok::And,
      "or" => Tok::Or,
      "not" => Tok::Not,

      "as" => Tok::As,
      "del" => Tok::Del,
      "is" => Tok::Is,
      "in" => Tok::In,
      "lambda" => Tok::Lambda,
      "from" => Tok::From,
      "import" => Tok::Import,
      "global" => Tok::Global,
      "nonlocal" => Tok::Nonlocal,

      "async" => Tok::Async,
      "await" => Tok::Await,

      "pass" => Tok::Pass,
      "assert" => Tok::Assert,
      "break" => Tok::Break,
      "continue" => Tok::Continue,
      "return" => Tok::Return,
      "yield" => Tok::Yield,

      "for" => Tok::For,
      "while" => Tok::While,
      "if" => Tok::If,
      "elif" => Tok::Elif,
      "else" => Tok::Else,
      "with" => Tok::With,

      "try" => Tok::Try,
      "except" => Tok::Except,
      "finally" => Tok::Finally,
      "raise" => Tok::Raise,

      "class" => Tok::Class,
      "def" => Tok::Def,

      _ => Tok::Id(word)
    }
  }

  fn indent_size(s: &str) -> isize {
    let mut size = 0;
    for c in s.chars() {
      if c == ' ' {
        size += 1;
      }
      else {
        size = (size + 8) / 8 * 8;
      }
    }

    return size
  }

  fn match_and_consume<F>(text: &mut &'input str, pos: &mut usize, re: &Regex, action: F)
    -> Option<Result<(Tok<'input>, usize), LexicalError>>
      where F: Fn(&'input str) -> Result<Tok<'input>, LexicalError>
  {
    if let Some(mat) = re.find(text) {
      *pos += mat.end();
      let ret = action(&text[mat.start()..mat.end()]);
      *text = &text[mat.end()..];
      match ret {
        Ok(t) => Some(Ok((t, mat.end()))),
        Err(e) => Some(Err(e))
      }
    } else {
      None
    }
  }

  fn next_token(&mut self) -> Option<Item<'input>> {
    loop {
      if self.chars.len() == 0 {
        return None
      }
      else if let Some(mat) = NEWLINE.find(self.chars) {
        self.state = LexerState::Begin;
        let prev_line = self.line;
        for c in mat.as_str().chars() {
          if c == '\n' || c == '\r' {
            self.line += 1;
          }
        }
        let start = self.column;
        self.column = 1;
        self.pos += mat.end();
        self.chars = &self.chars[mat.end()..];
        return Some(Ok(((prev_line, start), Tok::Newline, (self.line, 0))));
      }
      else if let Some(mat) = COMMENT.find(self.chars) {
        self.pos += mat.end();
        self.column += mat.end();
        self.chars = &self.chars[mat.end()..];
      }
      else if let Some(mat) = WHITESPACE.find(self.chars) {
        let start = self.column;
        self.pos += mat.end();
        self.column += mat.end();
        self.chars = &self.chars[mat.end()..];
        if self.state == LexerState::Begin {
          self.state = LexerState::InLine;
          let current_size = Lexer::indent_size(mat.as_str());
          let top = *self.indent_stack.last().unwrap();
          if top < current_size {
            return Some(Ok(((self.line, start), Tok::Indent, (self.line, self.column))))
          }
          else if top > current_size {
            return Some(Ok(((self.line, start), Tok::Dedent, (self.line, self.column))))
          }
        }
        else if mat.end() == 0 {
          break;
        }
      }
      else {
        break;
      }
    }

    if self.chars.len() == 0 {
      return None
    }

    use std::str::FromStr;
    macro_rules! actions {
      ($($x:expr => $y:expr),*) => {
        $(
          match Lexer::match_and_consume(&mut self.chars, &mut self.pos, &$x, $y) {
            Some(Ok((t, count))) => {
              let r = Some(Ok(((self.line, self.column), t, (self.line, self.column + count))));
              self.column += count;
              return r
            },
            Some(Err(e)) => {
              return Some(Err(e))
            }
            _ => {}
          }
        )*
        self.chars = &self.chars[self.chars.len()..];
        return Some(Err(LexicalError::Unrecognized))
      };
    }
    actions![
      ID        => |s:&'input str| Ok(Lexer::str_to_tok(s)),
      INTEGER   => |s:&'input str| {
        if &s[0..1] == "0" && s.len() > 1 {
          Err(LexicalError::Unrecognized)
        }
        else {
          match i32::from_str(s) {
            Ok(n) => Ok(Tok::Int(n)),
            _ => Err(LexicalError::IntegerOutOfRange),
          }
        }
      },
      STRING    => |s:&'input str| Ok(Tok::String(s)),
      EQ        => |_| Ok(Tok::Eq),
      NEQ       => |_| Ok(Tok::Neq),
      DIV       => |_| Ok(Tok::Div),
      LE        => |_| Ok(Tok::Le),
      GE        => |_| Ok(Tok::Ge),
      ARROW     => |_| Ok(Tok::Arrow),
      PLUS      => |_| Ok(Tok::Plus),
      MINUS     => |_| Ok(Tok::Minus),
      MUL       => |_| Ok(Tok::Mul),
      MOD       => |_| Ok(Tok::Mod),
      LT        => |_| Ok(Tok::Lt),
      GT        => |_| Ok(Tok::Gt),
      ASSIGN    => |_| Ok(Tok::Assign),
      LPAREN    => |_| Ok(Tok::LParen),
      RPAREN    => |_| Ok(Tok::RParen),
      LBRACKET  => |_| Ok(Tok::LBracket),
      RBRACKET  => |_| Ok(Tok::RBracket),
      COMMA     => |_| Ok(Tok::Comma),
      COLON     => |_| Ok(Tok::Colon),
      PERIOD    => |_| Ok(Tok::Period)
    ];
  }
}

impl<'input> Iterator for Lexer<'input> {
  type Item = Spanned<Tok<'input>, (usize, usize), LexicalError>;

  fn next(&mut self) -> Option<Self::Item> {
    return self.next_token();
  }
}

#[test]
fn regex_test() {
  assert_eq!(STRING.find(r#""asdf""#).unwrap().as_str(), r#""asdf""#);
  assert_eq!(STRING.find(r#""asd\nf""#).unwrap().as_str(), r#""asd\nf""#);
}
