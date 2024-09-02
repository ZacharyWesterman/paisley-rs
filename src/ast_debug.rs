use colored::Colorize;
use regex::Regex;
use std::fmt;

use crate::parser::ast::*;

impl std::fmt::Display for Program {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
		write!(f, "{}", self.print(0))
	}
}

trait PrintAST {
	fn print(&self, indent: usize) -> String;
}

impl PrintAST for Program {
	fn print(&self, indent: usize) -> String {
		self.stmts
			.iter()
			.map(|s| s.print(indent))
			.reduce(|a, b| a + &b)
			.unwrap_or(String::new())
	}
}

impl PrintAST for Statement {
	fn print(&self, indent: usize) -> String {
		let indent1 = "  ".repeat(indent + 1);
		let indent2 = "  ".repeat(indent + 2);

		"  ".repeat(indent)
			// + format!("({},{}) ", self.span.lo, self.span.hi).as_str()
			+ match &self.node {
				Stmt::Match(expr, branches, no_match) => {
					format!(
						"{}\n{}{}{}{}{}",
						"match".yellow(),
						expr.print(indent + 1),
						(indent1.to_string() + "branches\n").blue(),
						branches.iter().map(|s| s.print(indent+2)).reduce(|a,b| format!("{}{}", a, b)).unwrap_or(String::new()),
						(indent1.to_string() + "on no match").blue(),
						match no_match {
							Some(x) => "\n".to_string() + &x.print(indent+2),
							None => ", do nothing\n".blue().to_string(),
						},
					)
				}
				Stmt::Require(expr) => format!("{}\n{}", "require".yellow(), expr.print(indent + 1)),
				Stmt::If(condition, true_branch, false_branch) => format!(
					"{}\n{}{}{}{}{}",
					"if".yellow(),
					condition.print(indent+1),
					(indent1.to_string() + "if condition is true").blue(),
					match true_branch {
						Some(x) => "\n".to_string() + &x.print(indent+2),
						None => ", do nothing\n".blue().to_string(),
					},
					(indent1.to_string() + "if condition is false").blue(),
					match false_branch {
						Some(x) => "\n".to_string() + &x.print(indent+2),
						None => ", do nothing\n".blue().to_string(),
					},
				),
				Stmt::Command(expression) => "COMMAND\n".to_string(),
				_ => panic!("Unknown statement????"),
			}
			.as_str()
	}
}

impl PrintAST for Expression {
	fn print(&self, indent: usize) -> String {
		"  ".repeat(indent)
		// + format!("({},{}) ", self.span.lo, self.span.hi).as_str()
		+ "ERR\n"
	}
}
