use std::io::{stdout, Write};

use crate::execute::execute;

mod lexer;
mod parser;
mod execute;

fn main() {
  println!("rustlang REPL, enter some shit");
  println!("send input with a double enter");
  println!("exit with 'exit'");

  let stdin = std::io::stdin();

  let mut total_input = String::new();

  loop
  {
    let mut input = String::new();
    print!("> ");
    stdout().flush().unwrap();
    stdin.read_line(&mut input).unwrap();
    total_input += &input;
    if input == "\n"
    {
      if total_input == "quit\n\n" { break }

      let ast = parser::parse(&total_input);

      println!("{:#?}", &ast);

      match ast
      {
        Ok(v) => execute(v),
        Err(e) => println!("ERROR: {}", e)
      }

      total_input.clear();
    }
  }
}
