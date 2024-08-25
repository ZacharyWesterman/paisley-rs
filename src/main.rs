use std::fs;
use std::io;
use std::process::ExitCode;

mod flags;
mod lexer;
mod message;

fn main() -> ExitCode {
	//Disable colors globally if stderr or stdout are not TTY
	if !atty::is(atty::Stream::Stdout) || !atty::is(atty::Stream::Stderr) {
		colored::control::set_override(false);
	}

	let options = flags::read();

	//Read input file
	let mut s = String::new();
	let filename = if options.input.to_str().unwrap() == "-" {
		for line in io::stdin().lines() {
			s += &line.unwrap();
		}
		"stdin"
	} else {
		s = match fs::read_to_string(&options.input) {
			Ok(file_contents) => file_contents,
			Err(error) => {
				eprintln!("Error reading file {:?}: {}", options.input, error);
				return ExitCode::FAILURE;
			}
		};
		options.input.to_str().unwrap()
	}
	.to_string()
	.replace("\t", " "); //For easy formatting in error reporting, replace all tabs with spaces.

	let context = message::Context {
		filename: &filename,
		source: &s,
	};

	let lexer = lexer::Lexer::new(&context);

	for (token, _span) in lexer.into_iter() {
		println!("{:?}", token);
	}

	ExitCode::SUCCESS
}
