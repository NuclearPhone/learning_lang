use std::collections::HashMap;
use std::ops::{Add, Sub, Mul, Div};

use crate::parser::{Node, TypeName, RootUnit, FunctionDef};
use crate::lexer::Token;

enum VirtualValue
{
  Void,
  Number(f64),
  String(String)
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
          println!("added two numbers, {} + {} = {}", n, rn, n + rn);
          return VirtualValue::Number(n + rn)
        } else {
          panic!("attempted to add a number to a non-number value");
        }
      },
      VirtualValue::String(_) => {
        panic!("attempted to add a string to another value");
      }
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
          println!("subtracted two numbers, {} - {} = {}", n, rn, n - rn);
          return VirtualValue::Number(n - rn)
        } else {
          panic!("attempted to subtract a number to a non-number value");
        }
      },
      VirtualValue::String(_) => {
        panic!("attempted to subtract a string to another value");
      }
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
          println!("multiplyed two numbers, {} * {} = {}", n, rn, n * rn);
          return VirtualValue::Number(n * rn)
        } else {
          panic!("attempted to multiply a number to a non-number value");
        }
      },
      VirtualValue::String(_) => {
        panic!("attempted to multiply a string to another value");
      }
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
          println!("divided two numbers, {} / {} = {}", n, rn, n / rn);
          return VirtualValue::Number(n / rn)
        } else {
          panic!("attempted to divide a number to a non-number value");
        }
      },
      VirtualValue::String(_) => {
        panic!("attempted to divide a string to another value");
      }
    }
      
  }
}

#[derive(Default)]
struct Variable
{
  name: String,
  vartype: TypeName,
  value: VirtualValue
}

#[derive(Default)]
struct Variables
{
  parent: Option<Box<Variables>>,
  locals: HashMap<String, Variable>
}

struct VirtualMachine<'a>
{
  tree: &'a RootUnit,
  vars: Variables
}

impl <'a> VirtualMachine <'a>
{
  fn get_root(self: &Self) -> &RootUnit
  {
    &self.tree
  }
}

fn execute_node<'a> (mut vm: VirtualMachine<'a>, node: &Node) 
-> (VirtualMachine<'a>, VirtualValue) 
{
  match node
  {
    Node::Block(block) =>
    {
      for node in block
      {
        (vm, _) = execute_node(vm, node);
      }
      (vm, VirtualValue::Void)
    },
    Node::Binary{ left, right, op } =>
    {
      let (vm, left) = execute_node(vm, left);
      let (vm, right) = execute_node(vm, right);
      match op
      {
        Token::Plus => (vm, left + right),
        Token::Minus => (vm, left - right),
        Token::Asterisk => (vm, left * right),
        Token::Solidus => (vm, left / right),
        _ => panic!()
      }
    },
    Node::TokenVal(val) =>
    {
      match val
      {
        Token::Number(n) => (vm, VirtualValue::Number(*n)),
        _ => panic!("invalid token in execute_node procedure")
      }
    },
    _ => panic!("unknown node type in execute procedure")
  }
}

fn execute_function<'a>(vm: VirtualMachine<'a>, func: &FunctionDef) 
-> (VirtualMachine<'a>, VirtualValue)
{
  execute_node(vm, func.get_exec())
}

pub
fn execute(root: RootUnit)
{
  let vm: VirtualMachine = VirtualMachine
  {
    vars: Variables::default(),
    tree: &root
  };

  if let Some(func) = vm.tree.get_funcs().get("main")
  {
    _ = execute_function(vm, func);
  } else { panic!("unable to find function 'main'"); }
}

