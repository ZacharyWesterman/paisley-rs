use std::path::PathBuf;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(
	name = "Paisley Compiler",
	about = "A shell language with consistent syntax and a complete standard library."
)]
pub struct Options {
	/// The input file
	#[structopt(parse(from_os_str))]
	pub input: PathBuf,
}

pub fn read() -> Options {
	return Options::from_args();
}
