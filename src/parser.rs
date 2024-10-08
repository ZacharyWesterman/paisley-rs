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
		VarDecl(bool, Box<Vec<Ident>>, Option<Box<Statement>>),
		Subroutine(Box<String>, Box<Program>),
		While(Box<Expression>, Box<Program>),
		For(Box<Ident>, Box<Expression>, Box<Program>),
		ForKV(Box<Ident>, Box<Ident>, Box<Expression>, Box<Program>),
		Break(i64),
		Continue(i64),
		If(Box<Expression>, Option<Box<Program>>, Option<Box<Program>>),
		Match(Box<Expression>, Box<Vec<Statement>>, Option<Box<Program>>),
		Gosub(Box<Expression>),
		Define(Box<Expression>),
		Require(Box<Expression>),
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
		FuncCall(Box<Ident>, Box<Expression>),
		LambdaDecl(Box<String>, Box<Expression>),
		Lambda(Box<String>),
		Index(Box<Expression>, Box<Expression>),
		InlineCommand(Box<Expression>),
		DotIndex(Box<Expression>, Box<Expression>),
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

		//Subroutine declarations
		KwdSubroutine Text(name) program[pgm] KwdEnd => Statement {
			span: span!(),
			node: Stmt::Subroutine(Box::new(name), Box::new(pgm)),
		},

		//Variable declarations
		KwdLet identlist[vars] OperAssign statement[stmt] => Statement {
			span: span!(),
			node: Stmt::VarDecl(false, Box::new(vars), Some(Box::new(stmt))),
		},
		KwdLet identlist[vars] => Statement {
			span: span!(),
			node: Stmt::VarDecl(false, Box::new(vars), None),
		},

		//"initial x = expr" only allows initializing a single variable at a time.
		KwdInitial ident[var] OperAssign statement[stmt] => Statement {
			span: span!(),
			node: Stmt::VarDecl(true, Box::new(vec![var]), Some(Box::new(stmt))),
		},

		//Loops
		KwdWhile expression[expr] KwdDo program[pgm] KwdEnd => Statement {
			span: span!(),
			node: Stmt::While(Box::new(expr), Box::new(pgm)),
		},

		KwdFor ident[var] KwdIn expression[expr] KwdDo program[pgm] KwdEnd => Statement {
			span: span!(),
			node: Stmt::For(Box::new(var), Box::new(expr), Box::new(pgm)),
		},

		KwdFor ident[var1] ident[var2] KwdIn expression[expr] KwdDo program[pgm] KwdEnd => Statement {
			span: span!(),
			node: Stmt::ForKV(Box::new(var1), Box::new(var2), Box::new(expr), Box::new(pgm)),
		},

		//Single-line statements
		KwdBreak Newline => Statement {
			span: span!(),
			node: Stmt::Break(1),
		},
		KwdBreak Number(loop_count) Newline => Statement {
			span: span!(),
			node: Stmt::Break(loop_count as i64),
		},

		KwdContinue Newline => Statement {
			span: span!(),
			node: Stmt::Break(1),
		},
		KwdContinue Number(loop_count) Newline => Statement {
			span: span!(),
			node: Stmt::Continue(loop_count as i64),
		},

		KwdDefine command[expr] Newline => Statement {
			span: span!(),
			node: Stmt::Define(Box::new(Expression {
				span: span!(),
				node: Expr::Array(Box::new(expr)),
			})),
		},

		KwdRequire command[expr] Newline => Statement {
			span: span!(),
			node: Stmt::Require(Box::new(Expression {
				span: span!(),
				node: Expr::Array(Box::new(expr)),
			})),
		},

		KwdGosub expression[expr] Newline => Statement {
			span: span!(),
			node: Stmt::Gosub(Box::new(expr)),
		},

		//Conditionals
		KwdIf expression[condition] KwdThen program[true_branch] conditional_else[false_branch] => Statement {
			span: span!(),
			node: Stmt::If(Box::new(condition), if true_branch.stmts.len() == 0 { None } else { Some(Box::new(true_branch)) }, false_branch),
		},
		KwdIf expression[condition] KwdElse program[false_branch] KwdEnd => Statement {
			span: span!(),
			node: Stmt::If(Box::new(condition), None, if false_branch.stmts.len() == 0 { None } else { Some(Box::new(false_branch)) }),
		},

		KwdMatch expression[expr] KwdDo if_list[conditions] KwdElse program[no_match_branch] KwdEnd => Statement {
			span: span!(),
			node: Stmt::Match(Box::new(expr), Box::new(conditions), Some(Box::new(no_match_branch))),
		},
		KwdMatch expression[expr] KwdDo if_list[conditions] KwdEnd => Statement {
			span: span!(),
			node: Stmt::Match(Box::new(expr), Box::new(conditions), None),
		},

		//Commands
		command[x] Newline => Statement {
			span: span!(),
			node: Stmt::Command(Box::new(x)),
		},
	}

	conditional_else: Option<Box<Program>> {
		KwdEnd => None,
		KwdElse program[pgm] KwdEnd => Some(Box::new(pgm)),

		KwdElif expression[condition] KwdThen program[true_branch] conditional_else[false_branch] => Some(Box::new(Program {
			stmts: vec![
				Statement {
					span: span!(),
					node: Stmt::If(Box::new(condition), if true_branch.stmts.len() == 0 { None } else { Some(Box::new(true_branch)) }, false_branch),
				}
			],
		})),
	}

	if_list: Vec<Statement> {
		=> vec![],
		if_list[mut list] KwdIf expression[condition] KwdThen program[true_branch] KwdEnd => {
			list.push(Statement {
				span: span!(),
				node: Stmt::If(Box::new(condition), if true_branch.stmts.len() == 0 { None } else { Some(Box::new(true_branch)) }, None),
			});

			list
		}
		if_list[list] Newline => list,
	}

	command: Vec<Expression> {
		expression[e] => vec![e],

		command[mut lhs] CommandConcat expression[rhs] => {
			lhs.push(rhs);
			lhs
		},
	}

	identlist: Vec<Ident> {
		ident[x] => vec![x],
		identlist[mut lhs] OperConcat ident[rhs] => {
			lhs.push(rhs);
			lhs
		},
	}

	ident: Ident {
		Identifier(value) => Ident {
			span: span!(),
			value: value,
		}
	}

	expression: Expression {
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
		Command expression[a] RBrace => Expression{
			span: span!(),
			node: Expr::InlineCommand(Box::new(a)),
		},

		Quote expression[a] Quote => a,

		atom[lhs] LBracket expression[expr] RBracket => Expression {
			span:span!(),
			node: match lhs.node {
				Expr::Lambda(name) => Expr::LambdaDecl(name, Box::new(expr)),
				_ => Expr::Index(Box::new(lhs), Box::new(expr)),
			},
		},

		atom[lhs] Dot funcvar[rhs] => Expression {
			span: span!(),
			node: match rhs.node {
				Expr::FuncCall(funcname, mut func_expr) => Expr::FuncCall(funcname, {
					func_expr.node = match func_expr.node {
						Expr::Array(mut params) => {
							params.insert(0, lhs);
							Expr::Array(params)
						},
						_ => panic!("In dot func call, params are not an array?!"),
					};

					func_expr
				}),
				_ => Expr::DotIndex(Box::new(lhs), Box::new(rhs)),
			},
		},

		funcvar[x] => x,
	}

	funcvar: Expression {
		Identifier(i) => Expression {
			span: span!(),
			node: Expr::Variable(i),
		},

		ident[lhs] LParen expression[rhs] RParen => Expression {
			span: span!(),
			node: Expr::FuncCall(Box::new(lhs), Box::new(match rhs.node {
				Expr::Array(_) => rhs,

				_ => Expression {
					span: span!(),
					node: Expr::Array(Box::new(vec![rhs])),
				},
			})),
		},

		ident[lhs] LParen RParen => Expression {
			span: span!(),
			node: Expr::FuncCall(Box::new(lhs), Box::new(Expression {
				span: span!(),
				node: Expr::Array(Box::new(vec![])),
			})),
		},
	}
}

pub fn parse<I: Iterator<Item = (Token, Span)>>(
	i: I,
) -> Result<Program, (Option<(Token, Span)>, &'static str)> {
	parse_(i)
}
