use crossterm::{
  terminal::{self, enable_raw_mode, disable_raw_mode}, 
  event::{self, KeyCode},
  style::{self, Color, Stylize},
  queue,
  execute,
  cursor
};

use std::{io::Write,  time::Duration};

use crate::frontend::parser;
use crate::execute;

const DISPLAY_PREFIX: &'static str = "rustlang";

pub
fn run_repl()
{

  let mut stdout = std::io::stdout();

  execute!(stdout, terminal::EnterAlternateScreen).unwrap();
  
  queue!(stdout, style::PrintStyledContent("hello".with(style::Color::Yellow))).unwrap();

  stdout.flush().unwrap();

  let mut total_input = String::new();
  //let mut indentation = 0;

  // local functions
  fn display_line(line: &str)
  {
    let mut stdout = std::io::stdout();
    execute!(stdout,
      terminal::Clear(terminal::ClearType::CurrentLine)
    ).unwrap();

    queue!(stdout,
      style::PrintStyledContent(
        format!("\r{}> ", DISPLAY_PREFIX).with(Color::Yellow)
      ),
      style::Print(line)
    ).unwrap();
  }

  // main REPL loop
  loop
  {
    display_line("");

    stdout.flush().unwrap();

    let mut line_input = String::new();

    let mut display = String::new();

    // main line input loop
    enable_raw_mode().unwrap();
    loop
    {
      if event::poll(Duration::from_millis(5)).unwrap() != true 
      {
        continue;
      }

      if let event::Event::Key(key) = event::read().unwrap()
      {
        match key.code
        {
          KeyCode::Enter =>     break,
          KeyCode::Char(c) => 
          { 
            line_input.push(c);  
          },
          KeyCode::Backspace => 
          { 
            line_input.pop(); 
          },
          _ => {}
        }
        
        display_line(&line_input);

        stdout.flush().unwrap();
      }
    }

    disable_raw_mode().unwrap();

    execute!(
      stdout,
      cursor::MoveToNextLine(1)
    ).unwrap();

    total_input += &line_input;

    if line_input.is_empty()
    {
      let trimmed_input = total_input.trim();

      if trimmed_input == "quit" { break }

      let ast = parser::parse(trimmed_input);

      println!("{:#?}", &ast);
      
      match ast
      {
        Ok(v) => execute::execute(v),
        Err(e) => println!("ERROR: {}", e)
      }

      total_input.clear();
    }
  }

  terminal::disable_raw_mode().unwrap();

  queue!(stdout,
    terminal::LeaveAlternateScreen
  ).unwrap();
}
