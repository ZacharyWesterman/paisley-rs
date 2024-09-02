use std::fs;
use std::io;
use std::process::ExitCode;

mod ast_debug;
mod flags;
mod lexer;
mod message;
mod parser;

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
	.to_string();

	s = s.replace("\t", " "); //For easy formatting in error reporting, replace all tabs with spaces.

	let context = message::Context {
		filename: &filename,
		source: &s,
	};

	let lexer = lexer::Lexer::new(&context);

	if options.output_tokens {
		for (token, _span) in lexer.into_iter() {
			println!("{:?}", token);
		}
		return ExitCode::SUCCESS;
	}

	//Read input, splitting into tokens as it's read.
	let ast = match parser::parse(lexer) {
		Err(e) => {
			match e.0 {
				None => {
					//We hit EOF
					message::error(
						format!("{}", "Unexpected end of file"),
						None,
						Some(&context),
					);
				}
				Some(s) => {
					message::error(format!("{}", e.1), Some(s.1), Some(&context));
				}
			};

			None
		}
		Ok(program) => Some(program),
	};

	if message::errored() {
		message::abort();
		return ExitCode::FAILURE;
	}

	let ast = ast.unwrap();

	if options.output_ast {
		println!("{}", ast);
		return ExitCode::SUCCESS;
	}

	ExitCode::SUCCESS
}
