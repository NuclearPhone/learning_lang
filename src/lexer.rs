#[derive(PartialEq, Clone, Debug)]
pub
enum Token
{
  Nothing,
  Plus, Minus, Asterisk, Solidus,
  Equal, Colon,

  LeftParanthesis, RightParanthesis,
  LeftBrace, RightBrace,

  RightArrow,

  Identifier(String),
  Number(f64)
}

impl Default for Token
{
  fn default() -> Self 
  {
    Token::Nothing    
  }
}

pub
type Lexer = (String, Token);

pub
fn lex(stream: &str) -> Option<Lexer>
{
  let mut chars = stream.chars().peekable();

  while let Some(c) = chars.peek()
  {
    if !c.is_whitespace() { break; }
    else { chars.next(); }
  }

  match chars.peek()
  {
    Some(c) => {
      macro_rules! next_and_ret {
        ($iter: expr, $ret: expr) => { { $iter.next(); Some(($iter.collect::<String>(), $ret)) } }
      }
      match c
      {
        '+' => next_and_ret!(chars, Token::Plus),
        '*' => next_and_ret!(chars, Token::Asterisk),
        '/' => next_and_ret!(chars, Token::Solidus),
        '=' => next_and_ret!(chars, Token::Equal),
        '(' => next_and_ret!(chars, Token::LeftParanthesis),
        ')' => next_and_ret!(chars, Token::RightParanthesis),
        '{' => next_and_ret!(chars, Token::LeftBrace),
        '}' => next_and_ret!(chars, Token::RightBrace),

        '-' => {
          chars.next();

          if let Some(c) = chars.peek() {
            if *c == '>' { chars.next(); return Some((chars.collect::<String>(), Token::RightArrow)) } 
          }

          Some((chars.collect::<String>(), Token::Minus))
        },

        _ if c.is_alphabetic() =>
        {
          let mut out = String::new();

          while let Some(c) = chars.peek()
          {
            if !c.is_alphanumeric() { break }
            out.push(chars.next().unwrap());
          }

          Some((chars.collect::<String>(), Token::Identifier(out)))
        },
        _ if c.is_numeric() =>
        {
          let mut out = String::new();

          while let Some(c) = chars.peek()
          {
            if !c.is_numeric() && *c != '.' { break }
            else { out.push(chars.next().unwrap()); }
          }

          Some((chars.collect::<String>(), Token::Number(out.parse::<f64>().unwrap())))
        }
        _ => panic!("unknown character in lexer, faulting!")
      }
    },
    None => None
  }
}

#[macro_export]
macro_rules! lex_peek
{
  ($stream: expr, $peek: expr) => {
    {   
      assert!($peek >= 1);
      let mut counter = 0;
      let mut stream: String = $stream.clone();
      loop
      {
        counter += 1;

        if let Some(fwd) = lex(&stream) {
          let tok: Token;
          (stream, tok) = fwd;
          if counter == $peek {
            break Some((stream, tok))
          }
        } else {
          break None
        }
      }
    }
  }
}

