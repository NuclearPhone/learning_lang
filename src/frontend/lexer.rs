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
fn lex(stream: &str) -> Result<Lexer, String>
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
        ($iter: expr, $ret: expr) => { { $iter.next(); Ok(($iter.collect::<String>(), $ret)) } }
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
        '[' => next_and_ret!(chars, Token::LeftBracket),
        ']' => next_and_ret!(chars, Token::RightBracket),
        ',' => next_and_ret!(chars, Token::Comma),
        '.' => next_and_ret!(chars, Token::Period),
        '@' => next_and_ret!(chars, Token::At),
        ':' => next_and_ret!(chars, Token::Colon),
        ';' => next_and_ret!(chars, Token::Semicolon),

        '-' => {
          chars.next();

          if let Some(c) = chars.peek() {
            if *c == '>' { chars.next(); return Ok((chars.collect::<String>(), Token::RightArrow)) } 
          }

          Ok((chars.collect::<String>(), Token::Minus))
        },

        _ if c.is_alphabetic() =>
        {
          let mut out = String::new();

          while let Some(c) = chars.peek()
          {
            if !c.is_alphanumeric() { break }
            out.push(chars.next().unwrap());
          }

          Ok((chars.collect::<String>(), Token::Identifier(out)))
        },
        _ if c.is_numeric() =>
        {
          let mut out = String::new();

          while let Some(c) = chars.peek()
          {
            if !c.is_numeric() && *c != '.' { break }
            else { out.push(chars.next().unwrap()); }
          }

          Ok((chars.collect::<String>(), Token::Number(out.parse::<f64>().unwrap())))
        }
        _ => panic!("unknown character in lexer, faulting!")
      }
    },
    None => Err("ran out of tokens to parse".to_owned())
  }
}

#[macro_export]
macro_rules! lex_peek
{
  ($stream: expr, $peek: expr) => {
    {   
      assert!($peek >= 1);
      let mut counter = 0;
      let mut stream: String = $stream.to_owned();
      loop
      {
        counter += 1;

        if let Ok(fwd) = lex(&stream) {
          let tok: Token;
          (stream, tok) = fwd;
          if counter == $peek {
            break Ok((stream, tok))
          }
        } else {
          break Err("ran out of tokens to parse".to_owned())
        }
      }
    }
  }
}

