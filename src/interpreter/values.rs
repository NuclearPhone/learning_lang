use std::collections::HashMap;

use crate::frontend::parser::TypeName;

#[derive(Debug, Clone, PartialEq)]
pub
enum VirtualValue
{
  Void,
  Boolean(bool),
  Number(f64),
  String(String),
  Struct{ name: String, children: HashMap<String, VirtualValue> },
}

impl VirtualValue
{
  // gets the typename equivalent to the VV
  pub fn get_typename(&self) -> TypeName
  {
    match self
    {
      VirtualValue::Void => TypeName::Void,
      VirtualValue::Number(_) => TypeName::Numeric,
      VirtualValue::String(_) => TypeName::String,
      _ => panic!()
    }
  }
}

pub type OperationResult = Result<VirtualValue, String>;


pub fn add_virtual_values(lhs: &VirtualValue, rhs: &VirtualValue) -> OperationResult 
{
  match (lhs, rhs) 
  {
    (VirtualValue::Void, _) | (_, VirtualValue::Void) =>
      Err("attempted to perform a binary operation on a void value".to_owned())?,

    (VirtualValue::Number(lhs), VirtualValue::Number(rhs)) =>
      Ok(VirtualValue::Number(lhs + rhs)),

    _ => panic!()
  }
}

pub fn sub_virtual_values(lhs: &VirtualValue, rhs: &VirtualValue) -> OperationResult 
{
  match (lhs, rhs) 
  {
    (VirtualValue::Void, _) | (_, VirtualValue::Void) =>
      Err("attempted to perform a binary operation on a void value".to_owned())?,

    (VirtualValue::Number(lhs), VirtualValue::Number(rhs)) =>
      Ok(VirtualValue::Number(lhs - rhs)),

    _ => panic!()
  }
}

pub fn mul_virtual_values(lhs: &VirtualValue, rhs: &VirtualValue) -> OperationResult 
{
  match (lhs, rhs) 
  {
    (VirtualValue::Void, _) | (_, VirtualValue::Void) =>
      Err("attempted to perform a binary operation on a void value".to_owned())?,

    (VirtualValue::Number(lhs), VirtualValue::Number(rhs)) =>
      Ok(VirtualValue::Number(lhs * rhs)),

    _ => panic!()
  }
}

pub fn div_virtual_values(lhs: &VirtualValue, rhs: &VirtualValue) -> OperationResult 
{
  match (lhs, rhs) 
  {
    (VirtualValue::Void, _) | (_, VirtualValue::Void) =>
      Err("attempted to perform a binary operation on a void value".to_owned())?,

    (VirtualValue::Number(lhs), VirtualValue::Number(rhs)) =>
      Ok(VirtualValue::Number(lhs / rhs)),

    _ => panic!()
  }
}

pub fn lt_virtual_values(lhs: &VirtualValue, rhs: &VirtualValue) -> OperationResult 
{
  match (lhs, rhs) 
  {
    (VirtualValue::Void, _) | (_, VirtualValue::Void) =>
      Err("attempted to perform a binary operation on a void value".to_owned())?,

    (VirtualValue::Number(lhs), VirtualValue::Number(rhs)) =>
      Ok(VirtualValue::Boolean(lhs < rhs)),

    _ => panic!()
  }
}

pub fn gt_virtual_values(lhs: &VirtualValue, rhs: &VirtualValue) -> OperationResult
{
  match (lhs, rhs) 
  {
    (VirtualValue::Void, _) | (_, VirtualValue::Void) =>
      Err("attempted to perform a binary operation on a void value".to_owned())?,

    (VirtualValue::Number(lhs), VirtualValue::Number(rhs)) =>
      Ok(VirtualValue::Boolean(lhs > rhs)),

    _ => panic!()
  }
}

pub fn lte_virtual_values(lhs: &VirtualValue, rhs: &VirtualValue) -> OperationResult 
{
  match (lhs, rhs) 
  {
    (VirtualValue::Void, _) | (_, VirtualValue::Void) =>
      Err("attempted to perform a binary operation on a void value".to_owned())?,

    (VirtualValue::Number(lhs), VirtualValue::Number(rhs)) =>
      Ok(VirtualValue::Boolean(lhs == rhs)),

    _ => panic!()
  }
}

pub fn gte_virtual_values(lhs: &VirtualValue, rhs: &VirtualValue) -> OperationResult 
{
  match (lhs, rhs) 
  {
    (VirtualValue::Void, _) | (_, VirtualValue::Void) =>
      Err("attempted to perform a binary operation on a void value".to_owned())?,

    (VirtualValue::Number(lhs), VirtualValue::Number(rhs)) =>
      Ok(VirtualValue::Boolean(lhs == rhs)),

    _ => panic!()
  }

}

pub fn ne_virtual_values(lhs: &VirtualValue, rhs: &VirtualValue) -> OperationResult 
{
  match (lhs, rhs) 
  {
    (VirtualValue::Void, _) | (_, VirtualValue::Void) =>
      Err("attempted to perform a binary operation on a void value".to_owned())?,

    (VirtualValue::Number(lhs), VirtualValue::Number(rhs)) =>
      Ok(VirtualValue::Boolean(lhs != rhs)),

    (VirtualValue::String(lhs), VirtualValue::String(rhs)) =>
      Ok(VirtualValue::Boolean(lhs != rhs)),

    _ => panic!()
  }

}
pub fn eq_virtual_values(lhs: &VirtualValue, rhs: &VirtualValue) -> OperationResult 
{
  match (lhs, rhs) 
  {
    (VirtualValue::Void, _) | (_, VirtualValue::Void) =>
      Err("attempted to perform a binary operation on a void value".to_owned())?,

    (VirtualValue::Number(lhs), VirtualValue::Number(rhs)) =>
      Ok(VirtualValue::Boolean(lhs == rhs)),


    (VirtualValue::String(lhs), VirtualValue::String(rhs)) =>
      Ok(VirtualValue::Boolean(lhs == rhs)),

    _ => panic!()
  }
}

impl Default for VirtualValue
{
  fn default() -> Self {
     VirtualValue::Void
  }
}

