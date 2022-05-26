use std::collections::HashMap;

use crate::lex_peek;
use crate::lexer::{Token, lex};

#[derive(Debug)]
pub enum TypeName
{
  Void,
  Numeric,
  String,
  Custom(String)
}

#[derive(Debug)]
pub
enum Node
{
  TokenVal(Token),
  Block(Vec<Node>),
  Binary { left: Box<Node>, right: Box<Node>, op: Token },
  TypeDef, // unimplmemented
}

#[derive(Debug)]
pub struct FunctionDef
{
  name: String,
  rettype: TypeName,
  exec: Node
}

impl FunctionDef
{
  pub
  fn get_name(self: &Self) -> &String
  {
    return &self.name;
  }

  pub
  fn get_rettype(self: &Self) -> &TypeName
  {
    return &self.rettype;
  }

  pub
  fn get_exec(self: &Self) -> &Node
  {
    return &self.exec;
  }
}

#[derive(Debug)]
pub struct RootUnit
{
  funcs: HashMap<String, FunctionDef>
}

impl RootUnit
{
  pub
  fn get_funcs(self: &Self) -> &HashMap<String, FunctionDef>
  {
    return &self.funcs;
  }
}

impl Default for TypeName
{
  fn default() -> Self {
    TypeName::Void
  }
}

impl Node
{
  fn make_binary(left: Node, right: Node, op: &Token) -> Node
  {
    Node::Binary
    {
      left: Box::new(left),
      right: Box::new(right),
      op: op.clone()
    }
  }
}

type Parser<T> = (String, T);
type ParserResult<T> = Result<Parser<T>, String>;
type StringParser = Parser<String>;
type NodeParser = Parser<Node>;
type FunctionParser = Parser<FunctionDef>;
type UnitParser = Parser<RootUnit>;

fn parse_id(stream: &str) -> ParserResult<Node>
{
  if let Some((stream, s @ Token::Identifier(_))) = lex(&stream)
  {
    Ok((stream, Node::TokenVal(s)))
  }
  else
  {
    Err("Failed to parse an identifier".to_owned())
  }
}

fn parse_raw_id(stream: &str) -> ParserResult<String>
{
  if let Some((stream, Token::Identifier(s))) = lex(&stream)
  {
    Ok((stream, s))
  }
  else
  {
    Err("Failed to parse a raw identifier".to_owned())
  }
}

fn expect(stream: &str, tok_type: Token) -> Result<String, String>
{
  if let Some((stream, tok)) = lex(stream)
  {
    if tok == tok_type {
      Ok(stream)
    } else {
      Err(format!("Expected a token of type {:?}, but found {:?}", tok_type, tok))
    }
  } else {
    Err(format!("Expected a token of type {:?}, but ran out of tokens", tok_type))
  }
}

fn parse_factor(stream: &str) -> ParserResult<Node>
{
  if let Some((stream, tok)) = lex(&stream)
  {
    match tok 
    {
      Token::Identifier(_) | Token::Number(_) => 
        Ok((stream, Node::TokenVal(tok))),
      _ => Err("Failed to parse a factor".to_owned())
    }
  } else {
    Err("Ran out of things to parse in parse_factor".to_owned())
  }
}

fn parse_term(stream: &str) -> ParserResult<Node> 
{
  let (stream, left) = parse_factor(stream)?;

  if let Some((stream, op @ (Token::Asterisk | Token::Solidus))) = lex(&stream)
  {
    let (stream, right) = parse_term(&stream)?;
    return Ok((stream, Node::make_binary(left, right, &op)))
  }

  Ok((stream, left))
}

fn parse_expr(stream: &str) -> ParserResult<Node> 
{
  let (stream, left) = parse_term(stream)?;

  if let Some((stream, op @ (Token::Plus | Token::Minus))) = lex(&stream)
  {
    let(stream, right) = parse_expr(&stream)?;
    return Ok((stream, Node::make_binary(left, right, &op)))
  }

  Ok((stream, left))
}

fn parse_assignment(stream: &str) -> ParserResult<Node>
{
  if let Some((_, Token::Equal)) = lex_peek!(stream.to_owned(), 2)
  {
    let (stream, left)  = parse_id(&stream)?; 
    let stream = expect(&stream, Token::Equal)?;
    let (stream, right) = parse_expr(&stream)?;
    Ok((stream, Node::make_binary(left, right, &Token::Equal)))
  }
  else
  {
    parse_expr(stream)
  }
}

fn parse_statement_or_expr(stream: &str) -> ParserResult<Node>
{
  parse_assignment(stream)
}

fn parse_block_or_stexpr(stream: &str) -> ParserResult<Node>
{
  if let Some((stream, Token::LeftBrace)) = lex(stream)
  {
    let mut stream: String = stream;
    let mut nodes = vec![];

    loop {
      if let Some((break_stream, tok)) = lex(&stream) {
        if tok == Token::RightBrace { break stream = break_stream; }
        let (continue_stream, node) = parse_statement_or_expr(&stream)?;
        stream = continue_stream;
        nodes.push(node);
      } else {
        return Err("ran out of tokens while parsing a block".to_owned())
      }
    }

    Ok((stream.to_owned(), Node::Block(nodes)))
  } else { parse_statement_or_expr(stream) }
}

fn parse_type(stream: &str) -> ParserResult<TypeName>
{
  let (stream, id) = parse_raw_id(stream)?;
  match id.as_str()
  {
    "void"   => Ok((stream, TypeName::Void)),
    "number" => Ok((stream, TypeName::Numeric)),
    _ => Err("unknown typename".to_owned())
  }
}

fn parse_function_definition(stream: &str) -> ParserResult<FunctionDef>
{
  // messy, should write a dedicated "give me a basic string" function
  let (stream, name) = parse_raw_id(stream)?;
  
  // TODO function paranthesis
  let stream = expect(&stream, Token::LeftParanthesis)?;
  let stream = expect(&stream, Token::RightParanthesis)?; 

  let stream = expect(&stream, Token::RightArrow)?;
  let (stream, rettype) = parse_type(&stream)?;

  let (stream, exec) = parse_block_or_stexpr(&stream)?;

  Ok((
    stream,
    FunctionDef
    {
      name,
      rettype,
      exec
    }
  ))
}

pub fn parse(stream_in: &str) -> Result<RootUnit, String>
{
  let mut stream: String = stream_in.to_owned();

  let mut funcs: HashMap<String, FunctionDef> = HashMap::new();

  // run until dry
  while let Some(_) = lex(&stream)
  {
    if let Some((_, Token::LeftParanthesis)) = lex_peek!(&stream, 2)
    {
      let (new_stream, func) = parse_function_definition(&stream)?;
      stream = new_stream;
      if let None = funcs.get(&func.name)
      {
        funcs.insert(func.name.clone(), func);
      } else {
        return Err(format!("more than one function declaration of name {}", func.name));
      }
    } else {
      return Err("Unknown top level token".to_owned())
    }
  }

  Ok(
    RootUnit
    {
      funcs
    }
  )
}
