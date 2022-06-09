use std::collections::HashMap;

use super::lexer::{Token, lex, Lexer, lex_peek};

#[derive(Debug, PartialEq, Clone)]
pub enum TypeName
{
  Unset, // used for implicit types
  Void,
  Boolean,
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
  If { condition: Box<Node>, eval: Box<Node>, else_eval: Option<Box<Node>> },
  While { condition: Box<Node>, eval: Box<Node> },
  // TODO: for, needs a way to interact with iterators
  Binary { left: Box<Node>, right: Box<Node>, op: Token },
  VarDef {name: String, typename: TypeName, exec: Box<Node>},
  FuncCall {name: String, args: Vec<Node>},
  InternalFuncCall {name: String, args: Vec<Node>},
  TypeConstruction {typename: TypeName, child_constr: Vec<(String, Node)>}
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

impl FunctionDef
{
  pub
  fn get_name(self: &Self) -> &String
  {
    return &self.name;
  }

  pub
  fn get_params(&self) -> &Vec<(String, TypeName)>
  {
    &self.parameters
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

pub type Structure = Vec<(String, TypeName)>;

#[derive(Debug)]
pub struct TypeDef
{
  name: String,
  enumerations: HashMap<String, Structure>
}

#[derive(Debug)]
pub struct RootUnit
{
  funcs: HashMap<String, FunctionDef>,
  types: HashMap<String, TypeDef>
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

struct TestNodeCommon {
  line_id: usize,
  token_id: usize, 
}

enum TestNodeData {
  Token(Token, TestNodeCommon),
  Binary(Box<TestNode>, Box<TestNode>, TestNodeCommon) 
}

struct TestParser {
  lexer: Lexer,
}

type Parser<T> = (Lexer, T);
type ParserResult<T> = Result<Parser<T>, String>;

fn parse_id(lexer: Lexer) -> ParserResult<Node>
{
  if let Ok((lexer, s @ Token::Identifier(_))) = lex(lexer)
  {
    Ok((lexer, Node::TokenVal(s)))
  }
  else
  {
    Err("Failed to parse an identifier".to_owned())
  }
}

fn parse_raw_id(lexer: Lexer) -> ParserResult<String>
{
  match lex(lexer)
  {
    Ok((lexer, Token::Identifier(s))) => Ok((lexer, s)),
    Ok((_, x)) => Err(format!("Failed to parse a raw identifier, found {:?}", x)),
    Err(_) => Err(format!("Failed to parse a raw identifier, ran out of tokens."))
  }
}

fn expect(lexer: Lexer, tok_type: Token) -> Result<Lexer, String>
{
  if let Ok((lexer, tok)) = lex(lexer)
  {
    if tok == tok_type {
      Ok(lexer)
    } else {
      Err(format!("Expected a token of type {:?}, but found {:?}", tok_type, tok))
    }
  } else {
    Err(format!("Expected a token of type {:?}, but ran out of tokens", tok_type))
  }
}

// expect with custom error
fn expect_err(lexer: Lexer, tok_type: Token, fail: &str) -> Result<Lexer, String>
{
  match expect(lexer, tok_type)
  {
    o @ Ok(_) => o,
    Err(_) => Err(fail.to_owned())
  }
}

fn parse_input_parameters(lexer: Lexer) -> ParserResult<Vec<Node>>
{
  let mut lexer = expect_err(lexer, Token::LeftParanthesis, 
    "expected left paranthesis while attempting to parse input parameters for a function"
  )?;

  if let (lexer, Token::RightParanthesis) = lex(lexer)?
  {
    Ok((lexer, vec![]))
  } else {
    let mut params = vec![]; 
    loop
    {
      let node: Node;
      (lexer, node) = parse_expr(lexer)?;
      params.push(node);

      let tok: Token;
      (lexer, tok) = lex(lexer)?;

      if tok == Token::RightParanthesis { break Ok((lexer, params)) } 
      else { 
        lexer = expect_err(lexer, Token::Comma, 
          "expeted a comma after an identifier while attempting to 
          parse input parameters for a function"
        )? 
      }
    }
  }
}

fn parse_func_call(lexer: Lexer) -> ParserResult<Node>
{
  let (lexer, name) = parse_raw_id(lexer)?;
  
  // TODO: function parameters
  let (lexer, args) = parse_input_parameters(lexer)?;

  Ok((
    lexer,
    Node::FuncCall { name, args }
  ))
}

fn parse_internal_func_call(lexer: Lexer) -> ParserResult<Node>
{
  let lexer = expect(lexer, Token::At,)?;
  let (lexer, name) = parse_raw_id(lexer)?;
  
  let (lexer, args) = parse_input_parameters(lexer)?;

  Ok((
    lexer,
    Node::InternalFuncCall { name, args }
  ))
}

fn parse_type_constr(lexer: Lexer) -> ParserResult<Node>
{
  println!("a");
  let (lexer, typename) = parse_type(lexer)?;
  let mut lexer = expect(lexer, Token::LeftBrace)?;

  let mut child_constr = vec![];

  loop {
    if let (lexer, Token::RightBracket) = lex(lexer)? { 
      break Ok((lexer, Node::TypeConstruction{typename, child_constr}))
    } else {
      let name: String;
      let eval: Node;
      (lexer, name) = parse_raw_id(lexer)?;
      lexer = expect(lexer, Token::Colon)?;
      (lexer, eval) = parse_assignment(lexer)?;

      child_constr.push((name, eval));
    }
  }
}

fn parse_factor(lexer: Lexer) -> ParserResult<Node>
{
  if let Ok((used_lexer, tok)) = lex(lexer)
  {
    match tok 
    {
      Token::At =>
      {
        parse_internal_func_call(lexer)
      },
      Token::Identifier(_) =>
      {
        if let Ok((_, Token::LeftParanthesis)) = lex_peek(&lexer, 2) {
          parse_func_call(lexer)
        } 
        else if let Ok((_, Token::LeftBracket)) = lex_peek(&lexer, 2) {
          parse_type_constr(lexer)
        } else {
          Ok((used_lexer, Node::TokenVal(tok)))
        }
      },
      Token::Number(_) | Token::String(_) => 
        Ok((used_lexer, Node::TokenVal(tok))),
      Token::LeftParanthesis => {
        let (lexer, inside) = parse_assignment(used_lexer)?;
        let lexer = expect_err(lexer, Token::RightParanthesis, 
          "expected right paranthesis while parsing a factor"
        )?;
        Ok((lexer, inside))
      },
      _ => Err("Failed to parse a factor".to_owned())
    }
  } else {
    Err("Ran out of things to parse in parse_factor".to_owned())
  }
}

fn parse_term(lexer: Lexer) -> ParserResult<Node> 
{
  let (mut lexer, mut left) = parse_factor(lexer)?;

  // peek forward, if it satisfies the condition feed the peek lexer back into the actual lexer
  while let Ok((peek_lexer, op @ (Token::Asterisk | Token::Solidus))) = lex(lexer)
  {
    let right: Node;
    (lexer, right) = parse_factor(peek_lexer)?;
    left = Node::make_binary(left, right, &op);
  }

  Ok((lexer, left))
}

fn parse_expr(lexer: Lexer) -> ParserResult<Node> 
{
  let (mut lexer, mut left) = parse_term(lexer)?;

  while let Ok((peek_lexer, op @ (Token::Plus | Token::Minus))) = lex(lexer)
  {
    let right: Node;
    (lexer, right) = parse_term(peek_lexer)?;
    left = Node::make_binary(left, right, &op)
  }

  Ok((lexer, left))
}

fn parse_noteq(lexer: Lexer) -> ParserResult<Node>
{
  let (mut lexer, mut left) = parse_expr(lexer)?;

  while let Ok((peek_lexer, op @ (Token::LeftChevron | Token::RightChevron))) = lex(lexer)
  {
    let right: Node;
    (lexer, right) = parse_expr(peek_lexer)?;
    left = Node::make_binary(left, right, &op)
  }

  Ok((lexer, left))
}

fn parse_eq(lexer: Lexer) -> ParserResult<Node>
{
  let (mut lexer, mut left) = parse_noteq(lexer)?;

  while let Ok((peek_lexer, op @ (Token::Equalescent | Token::NotEqualescent))) = lex(lexer)
  {
    let right: Node;
    (lexer, right) = parse_noteq(peek_lexer)?;
    left = Node::make_binary(left, right, &op)
  }

  Ok((lexer, left))
}

fn parse_assignment(lexer: Lexer) -> ParserResult<Node>
{
  if let Ok((_, Token::Equal)) = lex_peek(&lexer, 2)
  {
    let (lexer, left)  = parse_id(lexer)?; 
    let lexer = expect(lexer, Token::Equal)?;
    let (lexer, right) = parse_eq(lexer)?;
    Ok((lexer, Node::make_binary(left, right, &Token::Equal)))
  }
  else
  {
    parse_eq(lexer)
  }
}

fn parse_vardef(lexer: Lexer) -> ParserResult<Node>
{
  let (lexer, name) = parse_raw_id(lexer)?;
  let mut lexer = expect(lexer, Token::Colon)?;

  let typename: TypeName;
  let mut exec: Box<Node> = Box::new(Node::Nothing);

  if let Ok((newlexer, parsed_type)) = parse_type(lexer) {
    lexer = newlexer;
    typename = parsed_type;
  } else { typename = TypeName::Void; }

  if let Ok(newlexer) = expect(lexer, Token::Equal) {
    let (newlexer, _exec) = parse_assignment(newlexer)?;
    exec = Box::new(_exec);
    lexer = newlexer;
  }

  Ok((
    lexer,
    Node::VarDef{ name, typename, exec }
  ))
}

fn parse_statement_or_expr(lexer: Lexer) -> ParserResult<Node>
{
  if let (_, Token::Colon) = lex_peek(&lexer, 2)? {
    parse_vardef(lexer)
  } else {
    let (lexer, condition) = parse_assignment(lexer)?;

    // IF parsing
    if let (lexer, Token::Question) = lex(lexer)? {
      let (lexer, eval) = parse_assignment(lexer)?;

      if let (lexer, Token::Bang) = lex(lexer) ? {
        let (lexer, else_eval) = parse_statement_or_expr(lexer)?;

        Ok((
          lexer,
          Node::If {
            condition: Box::new(condition),
            eval: Box::new(eval),
            else_eval: Some(Box::new(else_eval))
          }
        ))
      } else {
        Ok((
          lexer, 
          Node::If {
            condition: Box::new(condition),
            eval: Box::new(eval),
            else_eval: None
          }
        ))
      }
    }
    // WHILE PARSING
    else if let (lexer, Token::RightArrow) = lex(lexer)? {
      let (lexer, eval) = parse_block_or_stexpr(lexer)?;

      Ok((
        lexer,
        Node::While{
          condition: Box::new(condition),
          eval: Box::new(eval)
        }
      ))
    }

    // if no statement, go to default parsing
    else {
      Ok((lexer, condition))
    }
  }
}

fn parse_block_or_stexpr(lexer: Lexer) -> ParserResult<Node>
{
  if let Ok((mut lexer, Token::LeftBrace)) = lex(lexer)
  {
    let mut nodes = vec![];

    loop {
      if let Ok((break_lexer, tok)) = lex(lexer) {
        if tok == Token::RightBrace { break lexer = break_lexer; }
        let (continue_lexer, node) = parse_statement_or_expr(lexer)?;
        lexer = continue_lexer;
        nodes.push(node);
      } else {
        return Err("ran out of tokens while parsing a block".to_owned())
      }
    }

    Ok((lexer, Node::Block(nodes)))
  } else { parse_statement_or_expr(lexer) }
}

fn parse_type(lexer: Lexer) -> ParserResult<TypeName>
{
  let (lexer, id) = parse_raw_id(lexer)?;
  match id.as_str()
  {
    "void"   => Ok((lexer, TypeName::Void)),
    "number" => Ok((lexer, TypeName::Numeric)),
    "bool"   => Ok((lexer, TypeName::Boolean)),
    "string" => Ok((lexer, TypeName::String)),
    _ => Err("unknown typename".to_owned())
  }
}

// parses a PARAMETER DECLARATION, not an input for parameters
fn parse_decl_parameters(lexer: Lexer) -> ParserResult<Vec<(String, TypeName)>>
{
  let mut lexer = expect(lexer, Token::LeftParanthesis)?;

  if let (lexer, Token::RightParanthesis) = lex(lexer)? { Ok((lexer, vec![])) }
  else {
    let mut parameters: Vec<(String, TypeName)> = vec![];

    loop
    {
      let parsed_type: TypeName;
      let name: String;

      (lexer, name) = parse_raw_id(lexer)?;
      lexer = expect_err(lexer, Token::Colon, "expected a type-colon in parameter declaration")?;
      (lexer, parsed_type) = parse_type(lexer)?;

      parameters.push((name, parsed_type));

      if let (lexer, Token::RightParanthesis) = lex(lexer)? {
        break Ok((lexer, parameters));
      } else { expect(lexer, Token::Comma)?; }
    }
  }
}

fn parse_function_definition(lexer: Lexer) -> ParserResult<FunctionDef>
{
  // messy, should write a dedicated "give me a basic string" function
  let (lexer, name) = parse_raw_id(lexer)?;
  
  let (lexer, parameters) = parse_decl_parameters(lexer)?;

  let mut attribs = FunctionAttribs
  {
    internal: false
  };

  let mut lexer = lexer;

  if let (attrib_lexer, Token::LeftBracket) = lex(lexer)?
  {
    lexer = attrib_lexer;
    loop
    {
      let tok: Token;
      (lexer, tok) = lex(lexer)?;
      if tok == Token::RightBracket { break }
      if let Token::Identifier(i) = tok
      {
        match i.as_str()
        {
          "internal" => attribs.internal = true,
          _ => return Err(format!("unknown attribute {} in function attribute declaration", i))
        }

        if let (comma_lexer, Token::Comma) = lex_peek(&lexer, 1)? { lexer = comma_lexer; }
      } else {
        return Err("unexpected token in function attribute declaration".to_owned())
      }
    }
  }

  let lexer = expect(lexer, Token::RightArrow)?;
  let (lexer, rettype) = parse_type(lexer)?;

  let (lexer, exec) = parse_block_or_stexpr(lexer)?;

  Ok((
    lexer,
    FunctionDef
    {
      name,
      parameters,
      rettype,
      exec
    }
  ))
}

fn parse_type_declaration(lexer: Lexer) -> ParserResult<TypeDef>
{
  let (lexer, name) = parse_raw_id(lexer)?;

  Err("abc".to_owned())
}

pub fn parse(input: &str) -> Result<RootUnit, String>
{
  let mut lexer = Lexer::new(input);

  let mut funcs: HashMap<String, FunctionDef> = HashMap::new();
  let mut types: HashMap<String, TypeDef> =  HashMap::new();

  // run until dry
  while let Ok(_) = lex_peek(&lexer, 1)
  {
    if let Ok((_, Token::LeftParanthesis)) = lex_peek(&lexer, 2)
    {
      let func: FunctionDef;
      (lexer, func) = parse_function_definition(lexer)?;
      
      if let None = funcs.get(&func.name) {
        funcs.insert(func.name.clone(), func);
      } else {
        return Err(format!("more than one function declaration of name {}", func.name));
      }
    } 
    else if let Ok((_, Token::LeftBrace)) = lex_peek(&lexer, 2) {
      let typedef: TypeDef;
      (lexer, typedef) = parse_type_declaration(lexer)?;

      if let None = types.get(&typedef.name) {
        types.insert(typedef.name.clone(), typedef);
      } else {
        return Err(format!("more than one structure declaration of name {}", typedef.name));
      }
    } else {
      return Err("Unknown top level token".to_owned())
    }
  }

  Ok(
    RootUnit
    {
      funcs,
      types
    }
  )
}
