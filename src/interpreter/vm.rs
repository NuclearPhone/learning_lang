use crate::frontend::parser::{Node, RootUnit, FunctionDef};
use crate::frontend::lexer::Token;

use super::stacks::FunctionFrame;
use super::values::{VirtualValue, add_virtual_values, sub_virtual_values, div_virtual_values, ne_virtual_values, mul_virtual_values, eq_virtual_values, lt_virtual_values, gt_virtual_values};

struct VirtualMachine<'a>
{
  tree: &'a RootUnit,
  frame: FunctionFrame,
  // TODO: globals
}

type VMReturn<'a, T> = Result<(VirtualMachine<'a>, T), String>;

fn internal_print<'a>(vm: VirtualMachine<'a>, args: &'a Vec<Node>)
-> VMReturn<'a, VirtualValue>
{
  let mut vm = vm;

  for arg in args {
    let val: VirtualValue;
    (vm, val) = execute_node(vm, arg)?;

    match val
    {
      VirtualValue::Void => println!("void"),
      VirtualValue::Number(v) => println!("{}", v),
      VirtualValue::String(s) => println!("{}", s),
      _ => {}
    }
  }
  Ok((vm, VirtualValue::Void))
}

fn call_func<'a>(vm: VirtualMachine<'a>, name: &'a str, args: &'a Vec<Node>) 
-> VMReturn<'a, VirtualValue>
{
  let (vm, func) = vm.get_func(name)?;

  let params = func.get_params();
  
  if params.len() != args.len() {
    return Err(format!("mismatched number of arguments passed to function {}", name))
  }

  // evaluate arguments, then push them as the parameters into
  //   a new variable scope

  let mut vm: VirtualMachine = vm;

  let mut evaluated_nodes = vec![];

  for i in 0..params.len() {
    let (param_name, param_type) = &params[i];

    let arg_val: VirtualValue;
    (vm, arg_val) = execute_node(vm, &args[i])?;
    
    if *param_type != arg_val.get_typename() {
      return Err(
        "attempted to pass a variable of the incorrect type into a function".to_owned()
      );
    } 

    evaluated_nodes.push((param_name, arg_val));
  }

  vm.frame.create(); 

  for (param_name, arg_val) in evaluated_nodes {
    vm.frame.variables.create(param_name, arg_val)?;
  }

  let (mut vm, val) = execute_node(vm, func.get_exec())?;

  // finally, drop the stack frame
  vm.frame.drop();

  Ok((vm, val))
}

fn execute_node<'a, 'b>(vm: VirtualMachine<'a>, node: &'a Node) 
-> VMReturn<'a, VirtualValue> 
{
  match node
  {
    Node::Nothing => Ok((vm, VirtualValue::Void)),
    Node::If { condition, eval, else_eval } =>
    {
      let (vm, cond_res) = execute_node(vm, condition)?;

      if let VirtualValue::Boolean(b) = cond_res {
        if b {
          execute_node(vm, eval)
        } else if let Some(n) = else_eval {
          execute_node(vm, n)
        } else {
          Ok((vm, VirtualValue::Void))
        }
      } else {
        Err("found a non-boolean expression in a branch condition".to_owned())
      }
    },
    Node::While { condition, eval } =>
    {
      let mut vm = vm;

      loop
      {
        let cond_res: VirtualValue;

        (vm, cond_res) = execute_node(vm, condition)?;
        if cond_res == VirtualValue::Boolean(false) { break }

        (vm, _) = execute_node(vm, eval)?;
      }

      Ok((vm, VirtualValue::Void))
    },
    Node::VarDef{name, exec, typename} =>
    {
      let (mut vm, eval_exec) = execute_node(vm, exec)?;
      
      // ensure that the typename aligns with the exec
      if eval_exec.get_typename() != *typename
      {
        return Err(format!("types do not match up for variable def {}", name));
      }

      vm.frame.variables.create(name, eval_exec)?;

      Ok((vm, VirtualValue::Void))
    },
    Node::Block(block) =>
    {
      let mut vm = vm;

      for node in block
      {
        (vm, _) = execute_node(vm, node)?;
      }

      Ok((vm, VirtualValue::Void))
    },
    Node::Binary{ left, right, op } =>
    {
      // assignments are special cases
      if op == &Token::Equal {
        if let Node::TokenVal(Token::Identifier(i)) = &**left {
          let (mut vm, right_eval) = execute_node(vm, right)?;
          vm.frame.variables.set(i, right_eval.clone())?;
          return Ok((vm, right_eval));
        }
      }

      let (vm, left)  = execute_node(vm, left)?;
      let (vm, right) = execute_node(vm, right)?;
      match op
      {
        Token::Plus =>      Ok((vm, add_virtual_values(&left, &right)?)),
        Token::Minus =>     Ok((vm, sub_virtual_values(&left, &right)?)),
        Token::Asterisk =>  Ok((vm, mul_virtual_values(&left, &right)?)),
        Token::Solidus =>   Ok((vm, div_virtual_values(&left, &right)?)),
        Token::Equalescent => Ok((vm, eq_virtual_values(&left, &right)?)),
        Token::NotEqualescent => Ok((vm, ne_virtual_values(&left, &right)?)),
        Token::LeftChevron => Ok((vm, lt_virtual_values(&left, &right)?)),
        Token::RightChevron => Ok((vm, gt_virtual_values(&left, &right)?)),
        _ => panic!()
      }
    },
    Node::InternalFuncCall{name, args} =>
    {
      match name.as_str()
      {
        "PRINT" => internal_print(vm, args),
        _ => Err(format!("unknown internal function {}", name))
      }
    },
    Node::FuncCall{name, args} =>
    {
      call_func(vm, name, args)
    },
    Node::TokenVal(val) =>
    {
      match val
      {
        Token::String(s) => Ok((vm, VirtualValue::String(s.clone()))),
        Token::Number(n) => Ok((vm, VirtualValue::Number(*n))),
        Token::Identifier(i) => {
          //println!("encountered identifier {}", i);
          let var = vm.frame.variables.find(i)?.clone();
          Ok((vm,
              var
          ))
        },
        _ => panic!("invalid token in execute_node procedure")
      }
    },
    _ => panic!("unknown node type in execute procedure")
  }
}

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

  
}

pub
fn execute(root: RootUnit)
{
  let mut vm: VirtualMachine = VirtualMachine
  {
    frame: FunctionFrame::default(),
    tree: &root
  };

  // add a bare frame
  vm.frame.create();

  call_func(vm, "main", &vec![]).unwrap();
}

