use crate::message;
use regex::Regex;

#[derive(Debug, Clone)]
pub enum Token {
	//Ignored Tokens
	Whitespace,
	Comment,
	Newline,
	Unknown(String),

	//Keywords
	KwdLet,
	KwdInitial,
	KwdFor,
	KwdWhile,
	KwdIn,
	KwdDo,
	KwdMatch,
	KwdIf,
	KwdThen,
	KwdElif,
	KwdElse,
	KwdSubroutine,
	KwdGosub,
	KwdEnd,
	KwdReturn,
	KwdBreak,
	KwdContinue,
	KwdStop,
	KwdDefine,

	//Keyword operators
	OperOr,
	OperAnd,
	OperXor,
	OperNot,
	OperExists,
	OperLike,
	OperConcat,

	//Values
	Text(String),
	Identifier(String),
	Number(f64),
	Boolean(bool),
	Null,

	//Language Structures
	Command,
	LParen,
	RParen,
	LBrace,
	RBrace,
	LBracket,
	RBracket,
	Comma,
	Arrow,
	Semicolon,
	Dot,
	Lambda(String),

	//Operators
	OperPlus,
	OperMinus,
	OperMult,
	OperDiv,
	OperMod,
	OperPower,
	OperAssign,
	OperLessThan,
	OperLessOrEqual,
	OperGreaterThan,
	OperGreaterOrEqual,
	OperEqual,
	OperNotEqual,
	OperLength,
	OperSlice,

	Quote,
}

pub struct Lexer<'a> {
	original: &'a str,
	remaining: &'a str,
	context: &'a message::Context<'a>,
	scopes: Vec<Scope>,
	ended: bool,
	prev_token: Token,
	deferred_token: Option<(Token, Span)>,
	in_quote: bool,
}

impl<'a> Lexer<'a> {
	pub fn new(context: &'a message::Context) -> Lexer<'a> {
		Lexer {
			original: context.source,
			remaining: context.source,
			context: context,
			scopes: vec![Scope::Default],
			ended: false,
			prev_token: Token::Newline,
			deferred_token: None,
			in_quote: false,
		}
	}

	fn this_scope(&self) -> Scope {
		return self.scopes[self.scopes.len() - 1];
	}
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
	pub lo: usize,
	pub hi: usize,
}

#[derive(Debug, Clone, Copy)]
pub enum Scope {
	Default,
	Assignment,
	String,
	Expression,
	Command,
	LoopVars,
}

fn parse_escape_codes(text: String) -> String {
	let escape_codes = vec![
		("n", "\n"),
		("t", "\t"),
		("\"", "\""),
		("'", "'"),
		("\\", "\\"),
		("r", "\r"),
		(" ", " "), //Non-breaking space
		("{", "{"),
		("}", "}"),
		("^-^", "<sprite=0>"),
		(":relaxed:", "<sprite=0>"),
		(":P", "<sprite=1>"),
		(":yum:", "<sprite=1>"),
		("<3", "<sprite=2>"),
		(":heart_eyes:", "<sprite=2>"),
		("B)", "<sprite=3>"),
		(":sunglasses:", "<sprite=3>"),
		(":D", "<sprite=4>"),
		(":grinning:", "<sprite=4>"),
		("^o^", "<sprite=5>"),
		(":smile:", "<sprite=5>"),
		("XD", "<sprite=6>"),
		(":laughing:", "<sprite=6>"),
		(":lol:", "<sprite=6>"),
		("=D", "<sprite=7>"),
		(":smiley:", "<sprite=7>"),
		(":sweat_smile:", "<sprite=9>"),
		("DX", "<sprite=10>"),
		(":tired_face:", "<sprite=10>"),
		(";P", "<sprite=11>"),
		(":stuck_out_tongue_winking_eye:", "<sprite=11>"),
		(":-*", "<sprite=12>"),
		(";-*", "<sprite=12>"),
		(":kissing_heart:", "<sprite=12>"),
		(":kissing:", "<sprite=12>"),
		(":rofl:", "<sprite=13>"),
		(":)", "<sprite=14>"),
		(":slight_smile:", "<sprite=14>"),
		(":(", "<sprite=15>"),
		(":frown:", "<sprite=15>"),
		(":frowning:", "<sprite=15>"),
	];

	let mut result = String::new();
	let mut prev_index = 0;
	for (index, _) in text.match_indices("\\") {
		result += &text[prev_index..index];

		let mut found = false;
		for (code, value) in escape_codes.iter() {
			if (text.len() > index + code.len())
				&& (&&text[(index + 1)..(index + 1 + code.len())] == code)
			{
				result += value;
				prev_index = index + 1 + code.len();
				found = true;
				break;
			}
		}

		if !found {
			result += "\\"; //For now, just reappend the backslash if user gave an unknown escape sequence.
			prev_index = index + 1;
		}
	}

	result + &text[prev_index..]
}

fn next_token(text: &str, scope: Scope) -> Option<(Token, &str)> {
	if text.len() == 0 {
		return None;
	}

	let pattern_list: Vec<Regex> = (match scope {
		Scope::Default => {
			vec![
				r"^let\b",
				r"^initial\b",
				r"^for\b",
				r"^while\b",
				r"^in\b",
				r"^do\b",
				r"^match\b",
				r"^if\b",
				r"^then\b",
				r"^elif\b",
				r"^else\b",
				r"^subroutine\b",
				r"^gosub\b",
				r"^end\b",
				r"^return\b",
				r"^break\b",
				r"^continue\b",
				r"^stop\b",
				r"^define\b",
				r"^[ \t\r]",              //Whitespace
				r"^[\n;]",                //Line endings
				r"^#.*",                  //Comments
				r"^\{",                   //Open expression
				r"^\}",                   //Close expression
				r"^\$\{",                 //Open inline command eval
				"^\"",                    //Interpolated string marker
				r"^'(\\'|[^'])*'",        //Non-interpolated string marker
				"^[^\"'{}$ \\t\\r\\n#]+", //Anything else
				".",                      //Sanity check
			]
		}
		Scope::String => {
			vec![
				r"^\{",       //Open expression
				r"^\}",       //Close expression
				r"^\$\{",     //Open inline command eval
				"^\"",        //Interpolated string marker
				"^[^\"{}$]+", //Anything else
				".",          //Sanity check
			]
		}
		Scope::Assignment => {
			vec![
				"^[a-zA-Z_][a-zA-Z_0-9]*", //Identifiers
				r"^[ \t\r]",               //Whitespace
				r"^[\n;]",                 //Line endings
				r"^#.*",                   //Comments
				r"\=",                     //Assignment
				".",                       //Anything else
			]
		}
		Scope::Expression => {
			vec![
				r"^let\b",
				r"^for\b",
				r"^in\b",
				r"^if\b",
				r"^else\b",
				r"^exists\b",
				r"^like\b",
				r"^and\b",
				r"^or\b",
				r"^xor\b",
				r"^not\b",
				r"^true\b",
				r"^false\b",
				r"^null\b",
				"^[a-zA-Z_][a-zA-Z_0-9]*",             //Identifiers
				r"^0x[0-9a-fA-F_]*(\.[0-9a-fA-F_]+)?", //Hex numbers
				r"^0b[01_]*(\.[01_]+)?",               //Binary numbers
				r"^[0-9][0-9_]*(\.[0-9_]+)?",          //Numbers
				r"^[ \t\r\n]",                         //Whitespace (line endings are ignored in expressions)
				r"^#.*",                               //Comments
				r"^\$\{",
				r"^\(",
				r"^\)",
				r"^\{",
				r"^\}",
				r"^\[",
				r"^\]",
				"^,",
				"^=>",
				"^;",
				r"^\.",
				r"^\+",
				"^-",
				r"^\*",
				"^/",
				"^%",
				"^<",
				"^<=",
				"^>",
				"^>=",
				"^==?",
				"^[!~]=",
				r"^\&",
				r"^:",
				r"^\^",
				r"^\.",
				r"^!+\w*",         //Lambda
				"^\"",             //Interpolated string marker
				r"^'(\\'|[^'])*'", //Non-interpolated string marker
				".",               //Anything else
			]
		}

		Scope::Command => vec![
			r"^gosub\b",
			r"^[ \t\r]",              //Whitespace
			r"^[\n;]",                //Line endings
			r"^#.*",                  //Comments
			r"^\{",                   //Open expression
			r"^\}",                   //Close expression
			r"^\$\{",                 //Open inline command eval
			"^\"",                    //Interpolated string marker
			r"^'(\\'|[^'])*'",        //Non-interpolated string marker
			"^[^\"'{}$ \\t\\r\\n#]+", //Anything else
			".",                      //Sanity check
		],

		Scope::LoopVars => vec![
			r"^let\b",
			r"^initial\b",
			r"^for\b",
			r"^while\b",
			r"^in\b",
			r"^do\b",
			r"^match\b",
			r"^if\b",
			r"^then\b",
			r"^elif\b",
			r"^else\b",
			r"^subroutine\b",
			r"^gosub\b",
			r"^end\b",
			r"^return\b",
			r"^break\b",
			r"^continue\b",
			r"^stop\b",
			r"^define\b",
			r"^[ \t\r]",               //Whitespace
			r"^[\n;]",                 //Line endings
			r"^#.*",                   //Comments
			r"^\{",                    //Open expression
			r"^\}",                    //Close expression
			r"^\$\{",                  //Open inline command eval
			"^\"",                     //Interpolated string marker
			r"^'(\\'|[^'])*'",         //Non-interpolated string marker
			"^[a-zA-Z_][a-zA-Z_0-9]*", //Identifiers
			"^[^\"'{}$ \\t\\r\\n#]+",  //Anything else
			".",                       //Sanity check
		],
	})
	.iter()
	.map(|s| Regex::new(s).unwrap())
	.collect();

	if let Some((pattern, capture)) = pattern_list
		.iter()
		.find(|m| m.is_match_at(text, 0))
		.map(|m| (m, m.find_at(text, 0).unwrap().as_str()))
	{
		let token = match &pattern.as_str()[1..] {
			//Ignored Tokens
			r"[ \t\r]" => Token::Whitespace,
			r"[ \t\r\n]" => Token::Whitespace,
			r"#.*" => Token::Comment,
			r"[\n;]" => Token::Newline,

			//Keywords
			r"let\b" => Token::KwdLet,
			r"initial\b" => Token::KwdInitial,
			r"for\b" => Token::KwdFor,
			r"while\b" => Token::KwdWhile,
			r"in\b" => Token::KwdIn,
			r"do\b" => Token::KwdDo,
			r"match\b" => Token::KwdMatch,
			r"if\b" => Token::KwdIf,
			r"then\b" => Token::KwdThen,
			r"elif\b" => Token::KwdElif,
			r"else\b" => Token::KwdElse,
			r"subroutine\b" => Token::KwdSubroutine,
			r"gosub\b" => Token::KwdGosub,
			r"end\b" => Token::KwdEnd,
			r"return\b" => Token::KwdReturn,
			r"break\b" => Token::KwdBreak,
			r"continue\b" => Token::KwdContinue,
			r"stop\b" => Token::KwdStop,
			r"define\b" => Token::KwdDefine,

			//Keyword operators
			r"exists\b" => Token::OperExists,
			r"like\b" => Token::OperLike,
			r"and\b" => Token::OperAnd,
			r"or\b" => Token::OperOr,
			r"xor\b" => Token::OperXor,
			r"not\b" => Token::OperNot,

			//Values
			"[^\"'{}$ \\t\\r\\n#]+" => {
				//Raw text that may be coerced to a number if possible.
				match capture.parse() {
					Ok(value) => Token::Number(value),
					Err(_) => Token::Text(capture.to_owned()),
				}
			}
			"[^\"{}$]+" => {
				//Text inside an interpolated string
				Token::Text(parse_escape_codes(capture.to_owned()))
			}
			r"'(\\'|[^'])*'" => {
				//Text inside a non-interpolated string
				Token::Text(parse_escape_codes(
					capture[1..(capture.len() - 1)].to_string(),
				))
			}
			"[a-zA-Z_][a-zA-Z_0-9]*" => Token::Identifier(capture.to_owned()),
			r"[0-9][0-9_]*(\.[0-9_]+)?" => Token::Number(capture.replace("_", "").parse().unwrap()),
			r"0x[0-9a-fA-F_]*(\.[0-9a-fA-F_]+)?" => {
				let num = &capture.replace("_", "")[2..];
				let parts: Vec<&str> = num.split(".").collect();

				let int = if !parts[0].is_empty() {
					u64::from_str_radix(parts[0], 16).unwrap() //We know this will be valid.
				} else {
					0
				};

				let frac = if parts.len() == 2 {
					let mut fraction = 0.0;
					let mut divisor = 1.0;

					for c in parts[1].chars() {
						let digit = c.to_digit(16).unwrap() as f64;
						divisor *= 16.0;
						fraction += digit / divisor; //We know digit will be valid.
					}

					fraction
				} else {
					0.0
				};

				Token::Number(int as f64 + frac)
			}
			r"0b[01_]*(\.[01_]+)?" => {
				let num = &capture.replace("_", "")[2..];
				let parts: Vec<&str> = num.split(".").collect();

				let int = if !parts[0].is_empty() {
					u64::from_str_radix(parts[0], 2).unwrap() //We know this will be valid.
				} else {
					0
				};

				let frac = if parts.len() == 2 {
					let mut fraction = 0.0;
					let mut divisor = 1.0;

					for c in parts[1].chars() {
						let digit = c.to_digit(2).unwrap() as f64;
						divisor *= 2.0;
						fraction += digit / divisor; //We know digit will be valid.
					}

					fraction
				} else {
					0.0
				};

				Token::Number(int as f64 + frac)
			}
			r"true\b" => Token::Boolean(true),
			r"false\b" => Token::Boolean(false),
			r"null\b" => Token::Null,

			//Language Structures
			r"\$\{" => Token::Command,
			r"\(" => Token::LParen,
			r"\)" => Token::RParen,
			r"\{" => Token::LBrace,
			r"\}" => Token::RBrace,
			r"\[" => Token::LBracket,
			r"\]" => Token::RBracket,
			"," => Token::Comma,
			"=>" => Token::Arrow,
			";" => Token::Semicolon,
			r"\." => Token::Dot,
			r"!+\w*" => Token::Lambda(capture.to_owned()),

			//Operators
			r"\+" => Token::OperPlus,
			"-" => Token::OperMinus,
			r"\*" => Token::OperMult,
			"/" => Token::OperDiv,
			"%" => Token::OperMod,
			r"\^" => Token::OperPower,
			"=" => Token::OperAssign,
			"<" => Token::OperLessThan,
			"<=" => Token::OperLessOrEqual,
			">" => Token::OperGreaterThan,
			">=" => Token::OperGreaterOrEqual,
			"==?" => Token::OperEqual,
			"[!~]=" => Token::OperNotEqual,
			r"\&" => Token::OperLength,
			":" => Token::OperSlice,

			"\"" => Token::Quote,

			//If none of the above, raise an error!
			_ => Token::Unknown(capture.to_owned()),
		};

		return Some((token, &text[capture.len()..]));
	} else {
		panic!("Pattern list is not exhaustive!");
	}
}

impl<'a> Iterator for Lexer<'a> {
	type Item = (Token, Span);
	fn next(&mut self) -> Option<(Token, Span)> {
		if let Some((tok, span)) = self.deferred_token.clone() {
			self.deferred_token = None;
			return Some((tok, span));
		}

		loop {
			//Grab the next token
			let (tok, span) =
				if let Some((tok, new_remaining)) = next_token(self.remaining, self.this_scope()) {
					let lo = self.original.len() - self.remaining.len();
					let hi = self.original.len() - new_remaining.len();
					self.remaining = new_remaining;
					(tok, Span { lo, hi })
				} else if self.ended {
					return None;
				} else {
					//Always append a newline to the end of the file.
					//This helps simplify syntax parsing.
					self.ended = true;
					return Some((
						Token::Newline,
						Span {
							lo: self.original.len(),
							hi: self.original.len(),
						},
					));
				};

			//Depending on what token we got, change the scope.
			match self.this_scope() {
				Scope::Assignment => match tok {
					Token::OperAssign => self.scopes.push(Scope::Default),
					Token::Newline => {
						self.scopes.pop();
					}
					_ => {}
				},

				Scope::Expression => match tok {
					Token::RBrace => {
						self.scopes.pop();
					}
					_ => {}
				},

				Scope::Command => match tok {
					Token::RBrace => {
						self.scopes.pop();
					}
					_ => {}
				},

				Scope::LoopVars => match tok {
					Token::KwdIn => {
						self.scopes.pop();
					}
					_ => {}
				},

				s => match tok {
					Token::Command => self.scopes.push(Scope::Command),
					Token::LBrace => self.scopes.push(Scope::Expression),

					Token::Quote => match s {
						Scope::String => {
							self.scopes.pop();
						}
						_ => self.scopes.push(Scope::String),
					},
					Token::KwdLet => self.scopes.push(Scope::Assignment),
					Token::KwdInitial => self.scopes.push(Scope::Assignment),

					Token::Newline => {
						if self.scopes.len() > 1 {
							match self.scopes[self.scopes.len() - 2] {
								Scope::Assignment => {
									self.scopes.pop();
									self.scopes.pop();
								}
								_ => {}
							}
						}
					}

					Token::KwdFor => {
						self.scopes.push(Scope::LoopVars);
					}

					_ => {}
				},
			};

			//Spit out the token, if it's one we want to keep.
			match tok {
				Token::Whitespace | Token::Comment => {
					continue;
				}

				Token::Unknown(text) => {
					message::error(
						format!("unexpected character `{}`", text),
						Some(span),
						Some(&self.context),
					);
					continue;
				}

				Token::Text(_)
				| Token::Identifier(_)
				| Token::Number(_)
				| Token::Boolean(_)
				| Token::Null
				| Token::Lambda(_)
				| Token::LBrace
				| Token::LParen
				| Token::Quote
				| Token::Command => match self.prev_token {
					Token::Text(_)
					| Token::Identifier(_)
					| Token::Number(_)
					| Token::Boolean(_)
					| Token::Null
					| Token::Lambda(_)
					| Token::RBrace
					| Token::RParen
					| Token::RBracket
					| Token::Quote => {
						let is_quote = match tok {
							Token::Quote => true,
							_ => false,
						};
						let prev_quote = match self.prev_token {
							Token::Quote => true,
							_ => false,
						};

						let is_funccall = match tok {
							Token::LParen => match self.prev_token {
								Token::Identifier(_) => true,
								_ => false,
							},
							_ => false,
						};

						self.prev_token = tok.clone();

						//Make sure quotes have OperConcat on the OUTSIDE of the string, not the inside.
						if is_quote {
							self.in_quote = !self.in_quote;
						}

						if is_funccall
							|| (is_quote && !self.in_quote)
							|| (prev_quote && self.in_quote)
						{
							return Some((tok, span));
						}

						self.deferred_token = Some((tok.clone(), span.clone()));
						return Some((Token::OperConcat, span));
					}
					_ => {
						match tok {
							Token::Quote => self.in_quote = !self.in_quote,
							_ => {}
						};

						self.prev_token = tok.clone();
						return Some((tok, span));
					}
				},

				Token::KwdEnd => {
					self.deferred_token = Some((tok, span.clone()));
					return Some((Token::Newline, span));
				}

				_ => {
					self.prev_token = tok.clone();
					return Some((tok, span));
				}
			}
		}
	}
}
