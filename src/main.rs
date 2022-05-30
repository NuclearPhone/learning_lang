use std::process::exit;

use frontend::parser::parse;

mod arguments;
mod frontend;
mod execute;
mod repl;

fn main() 
{
  let args = arguments::parse_args();

  // if a root file is undefined, enter repl mode
  if args.root_file.is_empty()
  {
    repl::run_repl();
  }

  // otherwise start parsing a root file
  if let Ok(_filedata) = std::fs::read(&args.root_file)
  {
    let filedata = String::from_utf8(_filedata).unwrap();

    if !filedata.is_ascii()
    {
      println!("file {} is not a valid binary ascii file", args.root_file);
      exit(1);
    }

    match parse(&filedata) {
      Ok(ast) => {
        if args.verbose { println!("{:#?}", ast) }

        execute::execute(ast);
      },
      Err(e) => {
        println!("{}", e);
        exit(1);
      }
    } 
  } else {
    println!("failed to open file {}", args.root_file);
  }
}
