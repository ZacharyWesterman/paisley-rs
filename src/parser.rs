pub mod ast {
	use crate::lexer::Span;

	#[derive(Debug)]
	pub struct Program {
		pub stmts: Vec<Statement>,
	}

	#[derive(Debug)]
	pub struct Statement {
		pub span: Span,
		pub node: Stmt,
	}

	#[derive(Debug)]
	pub enum Stmt {
		Command(Box<Vec<Expression>>),
	}

	#[derive(Debug)]
	pub struct Expression {
		pub span: Span,
		pub node: Expr,
	}

	#[derive(Debug)]
	pub enum Expr {
		//Arithmetic
		Neg(Box<Expression>),
		Add(Box<Expression>, Box<Expression>),
		Sub(Box<Expression>, Box<Expression>),
		Mult(Box<Expression>, Box<Expression>),
		Div(Box<Expression>, Box<Expression>),
		Mod(Box<Expression>, Box<Expression>),
		Pow(Box<Expression>, Box<Expression>),

		//Boolean operators
		And(Box<Expression>, Box<Expression>),
		Or(Box<Expression>, Box<Expression>),
		Xor(Box<Expression>, Box<Expression>),
		Not(Box<Expression>),

		//Boolean comparison
		LessThan(Box<Expression>, Box<Expression>),
		LessOrEqual(Box<Expression>, Box<Expression>),
		GreaterThan(Box<Expression>, Box<Expression>),
		GreaterOrEqual(Box<Expression>, Box<Expression>),
		Equal(Box<Expression>, Box<Expression>),
		NotEqual(Box<Expression>, Box<Expression>),

		//Special operators
		Search(Box<Expression>, Box<Expression>),
		Pattern(Box<Expression>, Box<Expression>),
		Exists(Box<Expression>),
		Slice(Box<Expression>, Box<Expression>),

		Array(Box<Vec<Expression>>),
		Object(Box<Vec<(Expression, Expression)>>),

		//List comprehension
		ListComp(
			Box<Expression>,
			Box<Ident>,
			Box<Expression>,
			Option<Box<Expression>>,
		),
		Ternary(Box<Expression>, Box<Expression>, Box<Expression>),

		//String concatenation
		Concat(Box<Vec<Expression>>),

		//Atoms
		Variable(String),
		String(String),
		Number(f64),
		Boolean(bool),
		Null,
		FuncCall(Box<Expression>, Box<Expression>),
		LambdaDecl(Box<String>, Box<Expression>),
		Lambda(Box<String>),
		Index(Box<Expression>, Box<Expression>),
	}

	#[derive(Debug)]
	pub struct Param {
		pub span: Span,
		pub name: Ident,
		pub datatype: Ident,
	}

	#[derive(Debug)]
	pub struct Ident {
		pub span: Span,
		pub value: String,
	}
}

use crate::lexer::Token::*;
use crate::lexer::*;
use ast::*;
use plex::parser;

parser! {
	fn parse_(Token, Span);

	//Combine two spans.
	(a, b) {
		Span {
			lo: a.lo,
			hi: b.hi,
		}
	}

	program: Program {
		statements[s] => Program { stmts: s }
	}

	statements: Vec<Statement> {
		=> vec![],
		statements[mut st] statement[e] => {
			st.push(e);
			st
		},
		statements[st] Newline => st,

	}

	statement: Statement {
		expression[e] Newline => Statement {
			span: span!(),
			node: Stmt::Command(Box::new(vec![e])),
		},
	}

	ident: Ident {
		Identifier(value) => Ident {
			span: span!(),
			value: value,
		}
	}

	expression: Expression {
		// arrayconcat[x] => Expression {
		// 	span: span!(),
		// 	node: Expr::Array(Box::new(x)),
		// },
		// objectconcat[x] => Expression {
		// 	span: span!(),
		// 	node: Expr::Object(Box::new(x)),
		// },
		concat[x] => Expression {
			span: span!(),
			node: Expr::Concat(Box::new(x)),
		},

		comprehension[x] => x,
	}

	objectconcat: Vec<(Expression, Expression)> {
		comprehension[lhs] Arrow comprehension[rhs] => vec![(lhs, rhs)],
		objectconcat[mut obj] Comma comprehension[lhs] Arrow comprehension[rhs] => {
			obj.push((lhs, rhs));
			obj
		}
	}

	//Array concatenation has the lowest precedence
	arrayconcat: Vec<Expression> {
		Comma => vec![],
		comprehension[lhs] Comma comprehension[rhs] => vec![lhs, rhs],
		arrayconcat[mut lhs] Comma comprehension[rhs] => {
			lhs.push(rhs);
			lhs
		},
	}

	//String concatenation has very low precedence
	concat: Vec<Expression> {
		comprehension[lhs] OperConcat comprehension[rhs] => vec![lhs, rhs],
		concat[mut lhs] OperConcat comprehension[rhs] => {
			lhs.push(rhs);
			lhs
		},
	}

	//List comprehension and ternary operator
	comprehension: Expression {
		comparison[value] KwdFor ident[var] KwdIn comparison[expr] => Expression {
			span: span!(),
			node: Expr::ListComp(Box::new(value), Box::new(var), Box::new(expr), None),
		},
		comparison[value] KwdFor ident[var] KwdIn comparison[expr] KwdIf comparison[condition] => Expression {
			span: span!(),
			node: Expr::ListComp(Box::new(value), Box::new(var), Box::new(expr), Some(Box::new(condition))),
		},

		comparison[true_branch] KwdIf comparison[condition] KwdElse comparison[false_branch] => Expression {
			span: span!(),
			node: Expr::Ternary(Box::new(condition), Box::new(true_branch), Box::new(false_branch)),
		},

		comparison[x] => x,
	}

	//Boolean comparison
	comparison: Expression {
		comparison[lhs] OperLessThan boolean[rhs] => Expression {
			span: span!(),
			node: Expr::LessThan(Box::new(lhs), Box::new(rhs)),
		},
		comparison[lhs] OperLessOrEqual boolean[rhs] => Expression {
			span: span!(),
			node: Expr::LessOrEqual(Box::new(lhs), Box::new(rhs)),
		},
		comparison[lhs] OperGreaterThan boolean[rhs] => Expression {
			span: span!(),
			node: Expr::GreaterThan(Box::new(lhs), Box::new(rhs)),
		},
		comparison[lhs] OperGreaterOrEqual boolean[rhs] => Expression {
			span: span!(),
			node: Expr::GreaterOrEqual(Box::new(lhs), Box::new(rhs)),
		},
		comparison[lhs] OperNotEqual boolean[rhs] => Expression {
			span: span!(),
			node: Expr::NotEqual(Box::new(lhs), Box::new(rhs)),
		},
		comparison[lhs] OperEqual boolean[rhs] => Expression {
			span: span!(),
			node: Expr::Equal(Box::new(lhs), Box::new(rhs)),
		},
		comparison[lhs] OperLike boolean[rhs] => Expression {
			span: span!(),
			node: Expr::Pattern(Box::new(lhs), Box::new(rhs)),
		},
		comparison[lhs] KwdIn boolean[rhs] => Expression {
			span: span!(),
			node: Expr::Search(Box::new(lhs), Box::new(rhs)),
		},

		boolean[x] => x,
	}

	//Boolean operators
	boolean: Expression {
		boolean[lhs] OperAnd term[rhs] => Expression {
			span: span!(),
			node: Expr::And(Box::new(lhs), Box::new(rhs)),
		},
		boolean[lhs] OperOr term[rhs] => Expression {
			span: span!(),
			node: Expr::Or(Box::new(lhs), Box::new(rhs)),
		},
		boolean[lhs] OperXor term[rhs] => Expression {
			span: span!(),
			node: Expr::Xor(Box::new(lhs), Box::new(rhs)),
		},

		OperNot term[value] => Expression {
			span: span!(),
			node: Expr::Not(Box::new(value)),
		},

		term[x] => x,
	}

	//Addition (lower precedence than multiplication)
	term: Expression {
		term[lhs] OperPlus factor[rhs] => Expression {
			span: span!(),
			node: Expr::Add(Box::new(lhs), Box::new(rhs)),
		},
		term[lhs] OperMinus factor[rhs] => Expression {
			span: span!(),
			node: Expr::Sub(Box::new(lhs), Box::new(rhs)),
		},
		factor[x] => x,
	}

	//Multiplication
	factor: Expression {
		factor[lhs] OperMult power[rhs] => Expression {
			span: span!(),
			node: Expr::Mult(Box::new(lhs), Box::new(rhs)),
		},
		factor[lhs] OperDiv power[rhs] => Expression {
			span: span!(),
			node: Expr::Div(Box::new(lhs), Box::new(rhs)),
		},
		factor[lhs] OperMod power[rhs] => Expression {
			span: span!(),
			node: Expr::Mod(Box::new(lhs), Box::new(rhs)),
		},

		power[x] => x,
	}

	//Power and slicing
	power: Expression {
		power[lhs] OperPower negate[rhs] => Expression {
			span: span!(),
			node: Expr::Pow(Box::new(lhs), Box::new(rhs)),
		},

		power[lhs] OperSlice negate[rhs] => Expression {
			span: span!(),
			node: Expr::Slice(Box::new(lhs), Box::new(rhs)),
		},

		negate[e] => e,
	}

	//High-precedence operations
	negate: Expression {
		OperMinus atom[e] => Expression {
			span: span!(),
			node: Expr::Neg(Box::new(e)),
		},

		atom[e] OperExists => Expression {
			span: span!(),
			node: Expr::Exists(Box::new(e)),
		},

		atom[e] => e,
	}

	//AST rules for any node that can be a single value in an expression.
	atom: Expression {
		Identifier(i) => Expression {
			span: span!(),
			node: Expr::Variable(i),
		},

		Text(value) => Expression {
			span: span!(),
			node: Expr::String(value),
		},

		Number(value) => Expression {
			span: span!(),
			node: Expr::Number(value),
		},

		Boolean(value) => Expression {
			span: span!(),
			node: Expr::Boolean(value),
		},

		Null => Expression {
			span: span!(),
			node: Expr::Null,
		},

		atom[lhs] LParen expression[rhs] RParen => Expression {
			span: span!(),
			node: Expr::FuncCall(Box::new(lhs), Box::new(rhs)),
		},

		atom[lhs] LParen RParen => Expression {
			span: span!(),
			node: Expr::FuncCall(Box::new(lhs), Box::new(Expression {
				span: span!(),
				node: Expr::Array(Box::new(vec![])),
			})),
		},

		Lambda(name) => Expression {
			span: span!(),
			node: Expr::Lambda(Box::new(name)),
		},

		//Arrays
		LBrace arrayconcat[a] RBrace => Expression {
			span: span!(),
			node: Expr::Array(Box::new(a)),
		},
		LBrace arrayconcat[a] Comma RBrace => Expression {
			span: span!(),
			node: Expr::Array(Box::new(a)),
		},
		LParen arrayconcat[a] RParen => Expression {
			span: span!(),
			node: Expr::Array(Box::new(a)),
		},
		LParen arrayconcat[a] Comma RParen => Expression {
			span: span!(),
			node: Expr::Array(Box::new(a)),
		},

		//Objects
		LBrace objectconcat[a] RBrace => Expression {
			span: span!(),
			node: Expr::Object(Box::new(a)),
		},
		LBrace objectconcat[a] Comma RBrace => Expression {
			span: span!(),
			node: Expr::Object(Box::new(a)),
		},
		LParen objectconcat[a] RParen => Expression {
			span: span!(),
			node: Expr::Object(Box::new(a)),
		},
		LParen objectconcat[a] Comma RParen => Expression {
			span: span!(),
			node: Expr::Object(Box::new(a)),
		},
		LBrace Arrow RBrace => Expression {
			span: span!(),
			node: Expr::Object(Box::new(vec![])),
		},
		LParen Arrow RParen => Expression {
			span: span!(),
			node: Expr::Object(Box::new(vec![])),
		},
		LBrace Arrow Comma RBrace => Expression {
			span: span!(),
			node: Expr::Object(Box::new(vec![])),
		},
		LParen Arrow Comma RParen => Expression {
			span: span!(),
			node: Expr::Object(Box::new(vec![])),
		},


		LParen expression[a] RParen => a,
		LBrace expression[a] RBrace => a,

		Quote expression[a] Quote => a,

		atom[lhs] LBracket expression[expr] RBracket => Expression {
			span:span!(),
			node: match lhs.node {
				Expr::Lambda(name) => Expr::LambdaDecl(name, Box::new(expr)),
				_ => Expr::Index(Box::new(lhs), Box::new(expr)),
			},
		},
	}
}

pub fn parse<I: Iterator<Item = (Token, Span)>>(
	i: I,
) -> Result<Program, (Option<(Token, Span)>, &'static str)> {
	parse_(i)
}

#[cfg(debug_assertions)]
use colored::Colorize;
#[cfg(debug_assertions)]
use regex::Regex;
#[cfg(debug_assertions)]
pub fn pretty(ast: &Program) -> String {
	let fluff = Regex::new(r"\n *[\)\}\]],?").unwrap();
	let spans = Regex::new(r"\n *(lo|hi)").unwrap();
	let other = Regex::new(r"((Literal|Var)\()\n *([^\n]+)").unwrap();

	let text = format!("{:#?}", ast).replace("    ", "  ");

	let s1 = fluff.replace_all(&text, "");
	let s2 = spans.replace_all(&s1, " $1");
	let s3 = other.replace_all(&s2, "$1 $3".bold().yellow().to_string());

	return s3.to_string();
}
