#[derive(Default)]
pub
struct Arguments
{
  pub verbose: bool,
  pub root_file: String,
}

pub
fn parse_args() -> Arguments
{
  let mut args = std::env::args();

  args.next(); // eat the program name
 
  let mut ret = Arguments::default();
  
  loop {
    if let Some(x) = args.next() {
      match x.as_str() {
        "--verbose" => ret.verbose = true, 
        _ =>  {
          if ret.root_file.is_empty() { ret.root_file = x; }
          else { println!("Tried to define two root files at once. halting."); std::process::exit(1); }
        }
      }
    } else { break }
  }

  ret
}


