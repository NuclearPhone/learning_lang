use std::collections::HashMap;

use super::values::VirtualValue;

#[derive(Default, Clone)]
pub struct VariableStack
{
  parent: Option<Box<VariableStack>>,
  locals: HashMap<String, VirtualValue>
}

impl VariableStack
{
  pub fn find(self: &Self, name: &str) -> Result<&VirtualValue, String>
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
  pub fn create(self: &mut Self, name: &str, val: VirtualValue) -> Result<(), String>
  {
    if let None = self.locals.get(name) {
      self.locals.insert(name.to_owned(), val);
      Ok(())
    } else {
      Err(format!("attempted to create two variables of the same name, {}", name))
    }
  }

  // reads the tree and attempts to set a vars value
  pub fn set(self: &mut Self, name: &str, val: VirtualValue) -> Result<(), String>
  {
    if let Some(_) = self.locals.get(name) {
      self.locals.insert(name.to_owned(), val);
      Ok(())
    } else {
      if self.parent.is_some() {
        self.parent.as_mut().unwrap().set(name, val)
      } else {
        Err(format!("cannot find variable of name {} while attempting to set its value", name))
      }
    }
  }
}

#[derive(Clone)]
pub struct FunctionFrame
{
  parent: Option<Box<FunctionFrame>>,
  pub variables: VariableStack
}

impl FunctionFrame
{
  pub fn create(&mut self) {
    *self = FunctionFrame {
      parent: Some(Box::new(self.clone())),
      variables: VariableStack::default()
    };
  }

  pub fn drop(&mut self) {
    *self = *self.parent.clone().unwrap();
  }
}

impl Default for FunctionFrame
{
  fn default() -> Self 
  {
    FunctionFrame
    {
      parent: None,
      variables: VariableStack::default()
    }
  }
}
