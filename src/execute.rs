use std::collections::HashMap;
use std::ops::{Add, Sub, Mul, Div};

use crate::frontend::parser::{Node, TypeName, RootUnit, FunctionDef};
use crate::frontend::lexer::Token;

#[derive(Debug, Clone)]
enum VirtualValue
{
  Void,
  Number(f64),
  String(String),
  Struct{ name: String, children: HashMap<String, VirtualValue> },
}

impl VirtualValue
{
  // gets the typename equivalent to the VV
  fn get_typename(&self) -> TypeName
  {
    match self
    {
      VirtualValue::Void => TypeName::Void,
      VirtualValue::Number(_) => TypeName::Numeric,
      _ => panic!()
    }
  }
}

impl Default for VirtualValue
{
  fn default() -> Self {
     VirtualValue::Void
  }
}

impl Add for VirtualValue
{
  type Output = Self;

  fn add(self: Self, rhs: Self) -> Self::Output 
  {
    match self
    {
      VirtualValue::Void => {
        panic!("attempted to add void to another value");
      },
      VirtualValue::Number(n) => {
        if let VirtualValue::Number(rn) = rhs {
          //print!("added two numbers, {} + {} = {}\r\n", n, rn, n + rn);
          return VirtualValue::Number(n + rn)
        } else {
          panic!("attempted to add a number to a non-number value");
        }
      },
      VirtualValue::String(_) => {
        panic!("attempted to add a string to another value");
      },
      _ => panic!()
    }
  }
}

impl Sub for VirtualValue
{
  type Output = Self;

  fn sub(self, rhs: Self) -> Self::Output 
  {
    match self
    {
      VirtualValue::Void => {
        panic!("attempted to subtract void to another value");
      },
      VirtualValue::Number(n) => {
        if let VirtualValue::Number(rn) = rhs {
          //print!("subtracted two numbers, {} - {} = {}\r\n", n, rn, n - rn);
          return VirtualValue::Number(n - rn)
        } else {
          panic!("attempted to subtract a number to a non-number value");
        }
      },
      VirtualValue::String(_) => {
        panic!("attempted to subtract a string to another value");
      },
      _ => panic!()
    }
      
  }
}

impl Mul for VirtualValue
{
  type Output = Self;

  fn mul(self, rhs: Self) -> Self::Output 
  {
    match self
    {
      VirtualValue::Void => {
        panic!("attempted to multiply void to another value");
      },
      VirtualValue::Number(n) => {
        if let VirtualValue::Number(rn) = rhs {
          //print!("multiplyed two numbers, {} * {} = {}\r\n", n, rn, n * rn);
          return VirtualValue::Number(n * rn)
        } else {
          panic!("attempted to multiply a number to a non-number value");
        }
      },
      VirtualValue::String(_) => {
        panic!("attempted to multiply a string to another value");
      },
      _ => panic!()
    }
      
  }
}

impl Div for VirtualValue
{
  type Output = Self;

  fn div(self, rhs: Self) -> Self::Output 
  {
    match self
    {
      VirtualValue::Void => {
        panic!("attempted to divide void to another value");
      },
      VirtualValue::Number(n) => {
        if let VirtualValue::Number(rn) = rhs {
          //print!("divided two numbers, {} / {} = {}\r\n", n, rn, n / rn);
          return VirtualValue::Number(n / rn)
        } else {
          panic!("attempted to divide a number to a non-number value");
        }
      },
      VirtualValue::String(_) => {
        panic!("attempted to divide a string to another value");
      },
      _ => panic!()
    }
      
  }
}

#[derive(Default, Clone)]
struct Variable
{
  vartype: TypeName,
  value: VirtualValue
}

#[derive(Default)]
struct Variables
{
  parent: Option<Box<Variables>>,
  locals: HashMap<String, VirtualValue>
}

impl Variables
{
  fn find(self: &Self, name: &str) -> Result<&VirtualValue, String>
  {
    if let Some(v) = self.locals.get(name) {
      Ok(v)
    } else {
      if let Some(vars) = &self.parent {
        vars.find(name)
      } else {
        Err(format!("cannot find variable of name {}", name))
      }
    }
  }

  // creates a variable in the local scope, and sets its value
  fn create(self: &mut Self, name: &str, val: VirtualValue) -> Result<(), String>
  {
    if let None = self.locals.get(name) {
      self.locals.insert(name.to_owned(), val);
      Ok(())
    } else {
      Err(format!("attempted to create two variables of the same name, {}", name))
    }
  }

  // reads the tree and attempts to set a vars value
  fn set(self: &mut Self, name: &str, val: VirtualValue) -> Result<(), String>
  {
    Ok(())
    
  }
}

struct VirtualMachine<'a>
{
  tree: &'a RootUnit,
  vars: Variables
}

type VMReturn<'a, T> = Result<(VirtualMachine<'a>, T), String>;

impl <'a> VirtualMachine <'a>
{
  fn get_tree(self: &Self) -> &'a RootUnit { &self.tree }

  fn get_func(self: Self, name: &'a str) -> VMReturn<&'a FunctionDef>
  {
    let tree: &'a RootUnit = self.get_tree();
    if let Some(func) = tree.get_funcs().get(name) {
      Ok((self, func))
    } else {
      Err(format!("failed to find function {}", name))
    }
  }

  fn call_func(self: Self, name: &'a str) -> VMReturn<VirtualValue>
  {
    let (vm, func) = self.get_func(name)?;

    let (vm, val) = Self::execute_node(vm, func.get_exec())?;

    Ok((vm, val))
  }

  fn internal_print(self: Self, args: &'a Vec<Node>)
  -> VMReturn<'a, VirtualValue>
  {
    let mut vm = self;
    for arg in args {
      let val: VirtualValue;
      (vm, val) = vm.execute_node(arg)?;

      match val
      {
        VirtualValue::Void => println!("void"),
        VirtualValue::Number(v) => println!("{}", v),
        _ => {}
      }
    }
    Ok((vm, VirtualValue::Void))
  }

  fn execute_node(self: Self, node: &'a Node) 
  -> VMReturn<VirtualValue> 
  {
    match node
    {
      Node::Nothing => Ok((self, VirtualValue::Void)),
      Node::VarDef{name, exec, typename} =>
      {
        let (mut vm, eval_exec) = self.execute_node(exec)?;
        
        // ensure that the typename aligns with the exec
        if eval_exec.get_typename() != *typename
        {
          return Err(format!("types do not match up for variable def {}", name));
        }

        vm.vars.create(name, eval_exec)?;

        Ok((vm, VirtualValue::Void))
      },
      Node::Block(block) =>
      {
        let mut vm: VirtualMachine = self;

        for node in block
        {
          (vm, _) = Self::execute_node(vm, node)?;
        }

        Ok((vm, VirtualValue::Void))
      },
      Node::Binary{ left, right, op } =>
      {
        let (vm, left)  = Self::execute_node(self, left)?;
        let (vm, right) = Self::execute_node(vm, right)?;
        match op
        {
          Token::Plus =>      Ok((vm, left + right)),
          Token::Minus =>     Ok((vm, left - right)),
          Token::Asterisk =>  Ok((vm, left * right)),
          Token::Solidus =>   Ok((vm, left / right)),
          _ => panic!()
        }
      },
      Node::InternalFuncCall{name, args} =>
      {
        match name.as_str()
        {
          "PRINT" => self.internal_print(args),
          _ => Err(format!("unknown internal function {}", name))
        }
      },
      Node::FuncCall{name, args} =>
      {
        Err("functions are unimplemented in the interpreter".to_owned())
      },
      Node::TokenVal(val) =>
      {
        match val
        {
          Token::Number(n) => Ok((self, VirtualValue::Number(*n))),
          Token::Identifier(i) => {
            //println!("encountered identifier {}", i);
            let var = self.vars.find(i)?.clone();
            Ok((self,
                var
            ))
          },
          _ => panic!("invalid token in execute_node procedure")
        }
      },
      _ => panic!("unknown node type in execute procedure")
    }
  }
}




pub
fn execute(root: RootUnit)
{
  let vm: VirtualMachine = VirtualMachine
  {
    vars: Variables::default(),
    tree: &root
  };

  vm.call_func("main").unwrap();
}

