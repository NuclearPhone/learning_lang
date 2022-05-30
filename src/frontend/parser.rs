use std::collections::HashMap;

use crate::lex_peek;
use super::lexer::{Token, lex};

#[derive(Debug, PartialEq, Clone)]
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
  Nothing,
  TokenVal(Token),
  Block(Vec<Node>),
  Binary { left: Box<Node>, right: Box<Node>, op: Token },
  VarDef {name: String, typename: TypeName, exec: Box<Node>},
  FuncCall {name: String, args: Vec<Node>},
  InternalFuncCall {name: String, args: Vec<Node>},
  TypeDef, // unimplmemented
}

#[derive(Debug)]
pub struct FunctionAttribs
{
  internal: bool, // implemented by the compiler
}

#[derive(Debug)]
pub struct FunctionDef
{
  name: String,
  parameters: Vec<(String, TypeName)>,
  rettype: TypeName,
  exec: Node,
}

#[derive(Debug)]
pub struct InternalFunctionDef
{
  name: String,
  parameters: Vec<(String, TypeName)>,
  rettype: TypeName
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
  funcs: HashMap<String, FunctionDef>,
  internal_funcs: HashMap<String, InternalFunctionDef>
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

fn parse_id(stream: &str) -> ParserResult<Node>
{
  if let Ok((stream, s @ Token::Identifier(_))) = lex(&stream)
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
  if let Ok((stream, Token::Identifier(s))) = lex(&stream)
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
  if let Ok((stream, tok)) = lex(stream)
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

// expect with custom error
fn expect_err(stream: &str, tok_type: Token, fail: &str) -> Result<String, String>
{
  match expect(stream, tok_type)
  {
    o @ Ok(_) => o,
    Err(_) => Err(fail.to_owned())
  }
}

fn parse_input_parameters(stream: &str) -> ParserResult<Vec<Node>>
{
  let mut stream = expect_err(&stream, Token::LeftParanthesis, 
    "expected left paranthesis while attempting to parse input parameters for a function"
  )?;

  if let (stream, Token::RightParanthesis) = lex(&stream)?
  {
    Ok((stream, vec![]))
  } else {
    let mut params = vec![]; 
    loop
    {
      let node: Node;
      (stream, node) = parse_expr(&stream)?;
      params.push(node);

      let tok: Token;
      (stream, tok) = lex(&stream)?;

      if tok == Token::RightParanthesis { break Ok((stream, params)) } 
      else { 
        stream = expect_err(&stream, Token::Comma, 
          "expeted a comma after an identifier while attempting to 
          parse input parameters for a function"
        )? 
      }
    }
  }
}

fn parse_func_call(stream: &str) -> ParserResult<Node>
{
  let (stream, name) = parse_raw_id(stream)?;
  
  // TODO: function parameters
  let (stream, args) = parse_input_parameters(&stream)?;

  Ok((
    stream,
    Node::FuncCall { name, args }
  ))
}

fn parse_internal_func_call(stream: &str) -> ParserResult<Node>
{
  let stream = expect(stream, Token::At,)?;
  let (stream, name) = parse_raw_id(&stream)?;
  
  let (stream, args) = parse_input_parameters(&stream)?;

  Ok((
    stream,
    Node::InternalFuncCall { name, args }
  ))
}

fn parse_factor(stream: &str) -> ParserResult<Node>
{
  if let Ok((_, Token::LeftParanthesis)) = lex_peek!(stream, 2) {
    parse_func_call(stream)
  }
  else if let Ok((_, Token::At)) = lex(&stream) {
    parse_internal_func_call(&stream)
  }
  else if let Ok((stream, tok)) = lex(&stream)
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
  let (mut stream, mut left) = parse_factor(stream)?;

  // peek forward, if it satisfies the condition feed the peek stream back into the actual stream
  while let Ok((peek_stream, op @ (Token::Asterisk | Token::Solidus))) = lex(&stream)
  {
    let right: Node;
    (stream, right) = parse_factor(&peek_stream)?;
    left = Node::make_binary(left, right, &op);
  }

  Ok((stream, left))
}

fn parse_expr(stream: &str) -> ParserResult<Node> 
{
  let (mut stream, mut left) = parse_term(stream)?;

  while let Ok((peek_stream, op @ (Token::Plus | Token::Minus))) = lex(&stream)
  {
    let right: Node;
    (stream, right) = parse_term(&peek_stream)?;
    left = Node::make_binary(left, right, &op)
  }

  Ok((stream, left))
}

fn parse_assignment(stream: &str) -> ParserResult<Node>
{
  if let Ok((_, Token::Equal)) = lex_peek!(stream.to_owned(), 2)
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

fn parse_vardef(stream: &str) -> ParserResult<Node>
{
  let (stream, name) = parse_raw_id(stream)?;
  let mut stream = expect(&stream, Token::Colon)?;

  let typename: TypeName;
  let mut exec: Box<Node> = Box::new(Node::Nothing);

  if let Ok((newstream, parsed_type)) = parse_type(&stream) {
    stream = newstream;
    typename = parsed_type;
  } else { typename = TypeName::Void; }

  if let Ok(newstream) = expect(&stream, Token::Equal) {
    let (newstream, _exec) = parse_assignment(&newstream)?;
    exec = Box::new(_exec);
    stream = newstream;
  }
  

  Ok((
    stream,
    Node::VarDef{ name, typename, exec }
  ))
}

fn parse_statement_or_expr(stream: &str) -> ParserResult<Node>
{
  if let (_, Token::Colon) = lex_peek!(stream, 2)? {
    parse_vardef(stream)
  } else {
    parse_assignment(stream)
  }
}

fn parse_block_or_stexpr(stream: &str) -> ParserResult<Node>
{
  if let Ok((stream, Token::LeftBrace)) = lex(stream)
  {
    let mut stream: String = stream;
    let mut nodes = vec![];

    loop {
      if let Ok((break_stream, tok)) = lex(&stream) {
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

// parses a PARAMETER DECLARATION, not an input for parameters
fn parse_decl_parameters(stream: &str) -> ParserResult<Vec<(String, TypeName)>>
{
  let mut stream = expect(stream, Token::LeftParanthesis)?;

  if let (stream, Token::RightParanthesis) = lex(&stream)? { Ok((stream, vec![])) }
  else {
    let mut parameters: Vec<(String, TypeName)> = vec![];

    loop
    {
      let parsed_type: TypeName;
      let name: String;

      println!("{}", stream);

      (stream, name) = parse_raw_id(&stream)?;
      stream = expect_err(&stream, Token::Colon, "expected a type-colon in parameter declaration")?;
      (stream, parsed_type) = parse_type(&stream)?;

      parameters.push((name, parsed_type));

      if let (stream, Token::RightParanthesis) = lex(&stream)? {
        break Ok((stream, parameters));
      } else { expect(&stream, Token::Comma)?; }
    }
  }
}

fn parse_function_definition(stream: &str) -> ParserResult<FunctionDef>
{
  // messy, should write a dedicated "give me a basic string" function
  let (stream, name) = parse_raw_id(stream)?;
  
  let (stream, parameters) = parse_decl_parameters(&stream)?;

  let mut attribs = FunctionAttribs
  {
    internal: false
  };

  let mut stream: String = stream; // redefine stream
    
  if let (attrib_stream, Token::LeftBracket) = lex(&stream)?
  {
    stream = attrib_stream;
    loop
    {
      let (attrib_stream, tok) = lex(&stream)?;
      if tok == Token::RightBrace { break stream = attrib_stream; }
      if let Token::Identifier(i) = tok
      {
        match i.as_str()
        {
          "internal" => attribs.internal = true,
          _ => return Err(format!("unknown attribute {} in function attribute declaration", i))
        }

        if let (comma_stream, Token::Comma) = lex(&stream)? { stream = comma_stream; }
      } else {
        return Err("unexpected token in function attribute declaration".to_owned())
      }
    }
  }

  let stream = expect(&stream, Token::RightArrow)?;
  let (stream, rettype) = parse_type(&stream)?;

  let (stream, exec) = parse_block_or_stexpr(&stream)?;

  Ok((
    stream,
    FunctionDef
    {
      name,
      parameters,
      rettype,
      exec
    }
  ))
}

fn parse_internal_function(stream: &str) -> ParserResult<InternalFunctionDef>
{
  let stream = expect(stream, Token::At)?;
  let (stream, id) = parse_raw_id(&stream)?;
  
  let (stream, parameters) = parse_decl_parameters(&stream)?;

  let stream = expect(&stream, Token::RightArrow)?;
  let (stream, typename) = parse_type(&stream)?;

  let stream = expect(&stream, Token::Semicolon)?;

  Ok((
    stream,
    InternalFunctionDef 
    {
      name: id,
      parameters,
      rettype: typename
    }
  ))
}

pub fn parse(stream_in: &str) -> Result<RootUnit, String>
{
  let mut stream: String = stream_in.to_owned();

  let mut funcs: HashMap<String, FunctionDef> = HashMap::new();
  let mut internal_funcs: HashMap<String, InternalFunctionDef> = HashMap::new();

  // run until dry
  while let Ok(_) = lex(&stream)
  {
    if let Ok((_, Token::At)) = lex_peek!(&stream, 1)
    {
      let (new_stream, func) = parse_internal_function(&stream)?;
      stream = new_stream;

      if let None = internal_funcs.get(&func.name) {
        internal_funcs.insert(func.name.clone(), func);
      } else {
        return Err(format!("more than one internal function declaration of name {}", func.name));
      }
    }
    else if let Ok((_, Token::LeftParanthesis)) = lex_peek!(&stream, 2)
    {
      let (new_stream, func) = parse_function_definition(&stream)?;
      stream = new_stream;
      
      if let None = funcs.get(&func.name) {
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
      funcs,
      internal_funcs
    }
  )
}
