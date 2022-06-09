#[derive(PartialEq, Clone, Debug)]
pub
enum Token
{
  Nothing,
  Plus, Minus, Asterisk, Solidus,
  Equal,

  At,

  Semicolon, Colon,

  Comma, Period,

  LeftParanthesis, RightParanthesis,
  LeftBracket, RightBracket,
  LeftBrace, RightBrace,
  LeftChevron, RightChevron,

  RightArrow,

  Equalescent, NotEqualescent,
  Bang, Question,

  Identifier(String),
  String(String),
  Number(f64)
}

impl Default for Token
{
  fn default() -> Self 
  {
    Token::Nothing    
  }
}

#[derive(Clone)]
pub struct Lexer 
{
  current_line: String, // used for printing debugging information 
  chars: String,
}

impl Lexer
{
  pub fn new(input: &str) -> Self
  {
    Lexer {
      current_line: String::new(),
      chars: input.to_owned()
    }
  }
  fn next_char(&mut self) -> Option<char> {
    if let Some(c) = self.chars.pop() {
      if c == '\n' {
        self.current_line.clear()
      } else {
        self.current_line.push(c)
      }

      Some(c)
    } else {
      None
    }
  }

  fn peek_char(&self) -> Option<char> {
    self.chars.chars().nth(0)
  }


}
pub fn lex(lexer: Lexer) -> Result<(Lexer, Token), String>
{
  while let Some(c) = lexer.next_char()
  {
    if !c.is_whitespace() { break; }
  }

  match lexer.peek_char()
  {
    Some(c) => {
      macro_rules! next_and_ret {
        ($iter: expr, $ret: expr) => { { 
          $iter.next_char(); 
          Ok(($iter, $ret)) 
        } }
      }
      match c
      {
        '+' => next_and_ret!(lexer, Token::Plus),
        '*' => next_and_ret!(lexer, Token::Asterisk),
        '/' => next_and_ret!(lexer, Token::Solidus),
        '(' => next_and_ret!(lexer, Token::LeftParanthesis),
        ')' => next_and_ret!(lexer, Token::RightParanthesis),
        '{' => next_and_ret!(lexer, Token::LeftBrace),
        '}' => next_and_ret!(lexer, Token::RightBrace),
        '[' => next_and_ret!(lexer, Token::LeftBracket),
        ']' => next_and_ret!(lexer, Token::RightBracket),
        '<' => next_and_ret!(lexer, Token::LeftChevron),
        '>' => next_and_ret!(lexer, Token::RightChevron),
        ',' => next_and_ret!(lexer, Token::Comma),
        '.' => next_and_ret!(lexer, Token::Period),
        '@' => next_and_ret!(lexer, Token::At),
        ':' => next_and_ret!(lexer, Token::Colon),
        ';' => next_and_ret!(lexer, Token::Semicolon),

        '?' => next_and_ret!(lexer, Token::Question),

        '"' => {
          lexer.next_char();

          let mut string = String::new();

          while let Some(c) = lexer.next_char() {
            if c == '"' { break }
            else { string.push(c); }
          }

          Ok((lexer, Token::String(string)))
        },

        '=' => {
          lexer.next_char();

          if let Some(c) = lexer.peek_char() {
            if c == '=' { 
              lexer.next_char();
              return Ok((lexer, Token::Equalescent))
            }
          } 

          return Ok((lexer, Token::Equal))
        },

        '!' => {
          lexer.next_char();

          if let Some(c) = lexer.peek_char() {
            if c == '=' { 
              lexer.next_char(); 
              return Ok((lexer, Token::NotEqualescent))
            }
          } 

          Ok((lexer, Token::Bang))
        },

        '-' => {
          lexer.next_char();

          if let Some(c) = lexer.peek_char() {
            if c == '>' { 
              lexer.next_char(); 
              return Ok((lexer, Token::RightArrow))
            } 
          }

          Ok((lexer, Token::Minus))
        },

        _ if c.is_alphabetic() =>
        {
          let mut out = String::new();

          while let Some(c) = lexer.peek_char()
          {
            if !c.is_alphanumeric() && !c.eq(&'_') { break }
            out.push(lexer.next_char().unwrap());
          }

          Ok((lexer, Token::Identifier(out)))
        },
        _ if c.is_numeric() =>
        {
          let mut out = String::new();

          while let Some(c) = lexer.peek_char()
          {
            if !c.is_numeric() && c != '.' { break }
            else { out.push(lexer.next_char().unwrap()); }
          }

          Ok((lexer, Token::Number(out.parse::<f64>().unwrap())))
        }
        _ => panic!("unknown character in lexer, faulting!")
      }
    },
    None => Err("ran out of tokens to parse".to_owned())
  }

    
}

pub fn lex_peek(lexer: &Lexer, fwd: usize) -> Result<(Lexer, Token), String>
{
  let mut lexer = (*lexer).clone();

  if fwd < 1 { return Err("put 0 into peek for lexer".to_owned()); }
  
  let mut res;

  for _ in 1..fwd {
    (lexer, res) = lex(lexer)?;
  }

  Ok((lexer, res))
}
