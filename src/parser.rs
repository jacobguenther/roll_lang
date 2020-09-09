// File: parser.rs

#[cfg(feature = "serialize")]
use serde::{Deserialize, Serialize};

use super::lexer::{
	lexeme::Lexeme,
	token::{Token, TokenT},
	Lexer, LexerT,
};

use super::ast::{number::*, *};

pub trait ParserT {
	fn new(source: &str) -> Parser;
	fn parse(&mut self) -> Root;
	fn parse_expression_string(source: &str) -> Result<Expression, ParseError>;
}
trait ParserPrivateT {
	fn parse_start(&mut self);

	fn parse_string_literal(&mut self) -> String;
	fn parse_comment(&mut self) -> Option<String>;
	fn parse_macro(&mut self) -> Result<Macro, ParseError>;

	fn parse_roll(&mut self) -> Result<Roll, ParseError>;
	fn parse_expression(&mut self) -> Result<Expression, ParseError>;
	fn parse_mul_div(&mut self) -> Result<MulDiv, ParseError>;
	fn parse_power(&mut self) -> Result<Power, ParseError>;
	fn parse_unary(&mut self) -> Result<Unary, ParseError>;
	fn parse_atom(&mut self) -> Result<Atom, ParseError>;

	fn parse_function(&mut self) -> Result<Function, ParseError>;

	fn parse_number(&mut self) -> Result<Number, ParseError>;
	fn parse_integer(&mut self) -> Result<Integer, ParseError>;
	fn parse_float(&mut self) -> Result<Float, ParseError>;

	fn parse_roll_query(&mut self) -> Result<RollQuery, ParseError>;

	fn parse_dice(&mut self) -> Result<Dice, ParseError>;
	fn parse_normal(&mut self) -> Result<Normal, ParseError>;
	fn parse_fate(&mut self) -> Result<Fate, ParseError>;
	fn parse_computed(&mut self) -> Result<Computed, ParseError>;
	fn parse_computed_helper(&mut self) -> Result<Expression, ParseError>;

	fn parse_modifiers(&mut self) -> Modifiers;
	fn parse_comparison(&mut self) -> Result<Comparison, ParseError>;
	fn parse_comparison_and_integer(&mut self)
		-> Result<(Comparison, Option<Integer>), ParseError>;
	fn parse_comparison_and_require_integer(&mut self)
		-> Result<(Comparison, Integer), ParseError>;

	fn parse_expanding(&mut self) -> Result<Expanding, ParseError>;
	fn parse_exploding(&mut self) -> Result<Exploding, ParseError>;
	fn parse_compounding(&mut self) -> Result<Compounding, ParseError>;
	fn parse_penetrating(&mut self) -> Result<Penetrating, ParseError>;

	fn parse_reroll(&mut self) -> Result<Reroll, ParseError>;
	fn parse_high_low(&mut self) -> Result<PostModifier, ParseError>;
	fn parse_successes(&mut self) -> Result<PostModifier, ParseError>;
	fn parse_cirtical(&mut self) -> Result<PostModifier, ParseError>;

	fn step_lexemes(&mut self);
	fn step_lexemes_skip_whitespace(&mut self);
	fn skip_whitespace(&mut self);

	fn current(&self) -> Result<&Lexeme, ParseError>;
	fn next(&self) -> Result<&Lexeme, ParseError>;
	fn current_as_option(&self) -> Option<&Lexeme>;
	fn next_as_option(&self) -> Option<&Lexeme>;

	fn match_current_to_punctuation(&mut self, punctuation: &str) -> Result<Token, ParseError>;
	fn match_current_to_punctuation_skip_whitespace(
		&mut self,
		punctuation: &str,
	) -> Result<Token, ParseError>;
	fn match_current_to_literal(&mut self, literal: &str) -> Result<Token, ParseError>;
	fn match_current_to_operator(&mut self, operator: &str) -> Result<Token, ParseError>;

	fn is_roll(&self) -> bool;
	fn is_inline_roll(&self) -> bool;
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
pub enum ParseError {
	UnexpectedToken(Token),
	ExpectedPunctuation(String),
	ExpectedInteger,
	DoesNotMatch,
	OutOfBounds,
	Unknown,
}
#[derive(Debug, Copy, Clone)]
enum State {
	Start,
	StringLiteral,
	Roll,
	Done,
}
impl Default for State {
	fn default() -> State {
		State::Start
	}
}
#[derive(Debug)]
pub struct Parser {
	state: State,
	lexemes: Vec<Lexeme>,
	current_index: usize,
}
impl ParserT for Parser {
	fn new(source: &str) -> Parser {
		Parser {
			state: State::default(),
			lexemes: Lexer::new(source).collect(),
			current_index: 0,
		}
	}
	fn parse(&mut self) -> Root {
		let mut root = Root::new();
		loop {
			match self.state {
				State::Start => self.parse_start(),
				State::StringLiteral => {
					if let Ok(my_macro) = self.parse_macro() {
						root.push(Node::Macro(my_macro));
						continue;
					}
					let string_literal = self.parse_string_literal();
					root.push(Node::StringLiteral(string_literal));
				}
				State::Roll => match self.parse_roll() {
					Ok(roll) => {
						root.push(Node::Roll(roll));
						self.state = State::Start;
					}
					Err(parse_error) => {
						root.push(Node::ParseError(parse_error.clone()));
						self.state = State::Done;
					}
				},
				State::Done => break,
			};
		}
		root
	}
	fn parse_expression_string(source: &str) -> Result<Expression, ParseError> {
		let mut parser = Parser::new(source);
		parser.state = State::Roll;
		parser.parse_expression()
	}
}
impl ParserPrivateT for Parser {
	fn parse_start(&mut self) {
		self.state = if self.current().is_err() {
			State::Done
		} else if self.is_roll() || self.is_inline_roll() {
			State::Roll
		} else {
			State::StringLiteral
		};
	}

	fn parse_string_literal(&mut self) -> String {
		let mut literal = String::new();
		loop {
			if self.current().is_err() {
				self.state = State::Done;
				break;
			} else if self.is_roll() || self.is_inline_roll() {
				self.state = State::Roll;
				break;
			} else if self.current().unwrap().token().source() == "#" {
				break;
			} else {
				literal.push_str(self.current().unwrap().source());
				self.step_lexemes();
			}
		}
		literal
	}
	fn parse_comment(&mut self) -> Option<String> {
		let _start_index = self.current_index;
		self.match_current_to_punctuation("[").ok()?;
		let mut comment = String::new();
		loop {
			match self.match_current_to_punctuation("]") {
				Ok(_token) => {
					self.skip_whitespace();
					break;
				}
				Err(_parse_error) => {
					comment.push_str(self.current().ok()?.source());
					self.step_lexemes();
				}
			};
		}
		Some(comment)
	}

	fn parse_macro(&mut self) -> Result<Macro, ParseError> {
		let start_index = self.current_index;
		self.skip_whitespace();
		if let Err(parse_error) = self.match_current_to_punctuation("#") {
			self.current_index = start_index;
			return Err(parse_error);
		};
		match self.match_current_to_punctuation("{") {
			Ok(_token) => {
				let mut macro_name = String::new();
				loop {
					match self.match_current_to_punctuation("}") {
						Ok(_token) => break,
						Err(_parse_error) => {
							macro_name.push_str(&self.current()?.token().source());
							self.current_index += 1;
						}
					}
				}
				return Ok(Macro { name: macro_name });
			}
			Err(_parse_error) => {
				if let Lexeme::Literal(token) = self.current()?.clone() {
					self.step_lexemes();
					return Ok(Macro {
						name: token.source().to_string(),
					});
				}
			}
		};
		self.current_index = start_index;
		Err(ParseError::DoesNotMatch)
	}
	fn parse_roll(&mut self) -> Result<Roll, ParseError> {
		if self.is_roll() {
			self.step_lexemes();
			self.step_lexemes();
			let expression = self.parse_expression()?;
			if self.match_current_to_punctuation("\\").is_err() {
				let start_index = self.current_index;
				self.skip_whitespace();
				if self.match_current_to_punctuation("\\").is_err() {
					self.current_index = start_index - 1;
					match self.current().unwrap() {
						Lexeme::Whitespace(_) => (),
						_ => self.current_index += 1,
					}
				}
			}
			Ok(Roll::ExplicitRoll(expression))
		} else {
			self.step_lexemes();
			self.step_lexemes();
			let expression = self.parse_expression()?;
			self.match_current_to_punctuation("]")?;
			self.match_current_to_punctuation("]")?;
			Ok(Roll::InlineRoll(expression))
		}
	}
	fn parse_expression(&mut self) -> Result<Expression, ParseError> {
		self.skip_whitespace();
		let mut expression = Expression::MulDiv(self.parse_mul_div()?);
		loop {
			let start_index = self.current_index;
			self.skip_whitespace();
			let is_add = match self.current() {
				Ok(lexeme) => match lexeme {
					Lexeme::Operator(token) => match token.source() {
						"+" => true,
						"-" => false,
						_ => {
							self.current_index = start_index;
							break; // wrong operator
						}
					},
					_ => break, // not an operator
				},
				Err(_parse_error) => break, // probably at EOF
			};
			self.step_lexemes_skip_whitespace();

			let next_mul_div = match self.parse_mul_div() {
				Ok(next) => next,
				Err(_parse_error) => {
					self.current_index = start_index;
					break;
				}
			};
			expression = match is_add {
				true => Expression::Add(Box::new(expression), next_mul_div),
				false => Expression::Subtract(Box::new(expression), next_mul_div),
			};
		}
		Ok(expression)
	}
	fn parse_mul_div(&mut self) -> Result<MulDiv, ParseError> {
		let mut mul_div = MulDiv::Power(self.parse_power()?);
		loop {
			let start_index = self.current_index;
			self.skip_whitespace();
			let is_multiply = match self.current() {
				Ok(lexeme) => match lexeme {
					Lexeme::Operator(token) => match token.source() {
						"*" => true,
						"/" => false,
						_ => {
							self.current_index = start_index;
							break; // wrong operator
						}
					},
					_ => break,
				},
				Err(_parse_error) => break,
			};

			self.step_lexemes_skip_whitespace();
			let next_power = match self.parse_power() {
				Ok(next) => next,
				Err(_parse_error) => {
					self.current_index = start_index;
					break;
				}
			};
			mul_div = match is_multiply {
				true => MulDiv::Multiply(Box::new(mul_div), next_power),
				false => MulDiv::Divide(Box::new(mul_div), next_power),
			};
		}
		Ok(mul_div)
	}
	fn parse_power(&mut self) -> Result<Power, ParseError> {
		let lhs = self.parse_unary()?;
		if let Ok(lexeme) = self.current() {
			if let Lexeme::Operator(token) = lexeme {
				if let "**" | "^" = token.source() {
					let start_index = self.current_index;
					self.step_lexemes();
					match self.parse_power() {
						Ok(power) => return Ok(Power::Pow(lhs, Box::new(power))),
						Err(_parse_error) => self.current_index = start_index,
					};
				}
			}
		};
		Ok(Power::Unary(lhs))
	}
	fn parse_unary(&mut self) -> Result<Unary, ParseError> {
		let start_index = self.current_index;
		let comment = self.parse_comment();
		match self.match_current_to_operator("-") {
			Ok(_token) => match self.parse_unary() {
				Ok(unary) => return Ok(Unary::Minus(comment, Box::new(unary))),
				Err(_parse_error) => self.current_index = start_index,
			},
			Err(_parse_error) => self.current_index = start_index,
		}
		Ok(Unary::Atom(
			self.parse_comment(),
			self.parse_atom()?,
			self.parse_comment(),
		))
	}
	fn parse_atom(&mut self) -> Result<Atom, ParseError> {
		let start_index = self.current_index;
		match self.parse_dice() {
			Ok(dice) => return Ok(Atom::Dice(dice)),
			Err(_parse_error) => self.current_index = start_index,
		};
		match self.parse_number() {
			Ok(num) => return Ok(Atom::Number(num)),
			Err(_parse_error) => self.current_index = start_index,
		};
		match self.parse_roll_query() {
			Ok(query) => return Ok(Atom::RollQuery(query)),
			Err(_parse_error) => self.current_index = start_index,
		}
		match self.parse_function() {
			Ok(function) => return Ok(Atom::Function(function)),
			Err(_parse_error) => self.current_index = start_index,
		};

		if let Err(parse_error) = self.match_current_to_punctuation_skip_whitespace("(") {
			self.current_index = start_index;
			return Err(parse_error);
		}
		let expression = match self.parse_expression() {
			Ok(expr) => expr,
			Err(parse_error) => {
				self.current_index = start_index;
				return Err(parse_error);
			}
		};
		if let Err(parse_error) = self.match_current_to_punctuation_skip_whitespace(")") {
			self.current_index = start_index;
			return Err(parse_error);
		}
		Ok(Atom::ParenthesesExpression(Box::new(expression)))
	}

	fn parse_function(&mut self) -> Result<Function, ParseError> {
		if let Lexeme::Literal(token) = self.current()?.clone() {
			if let "abs" | "ceil" | "floor" | "round" = token.source() {
				let start_index = self.current_index;
				self.step_lexemes();
				if let Err(parse_error) = self.match_current_to_punctuation_skip_whitespace("(") {
					self.current_index = start_index;
					return Err(parse_error);
				}

				let expression = Box::new(self.parse_expression()?);
				let function = match token.source() {
					"abs" => Function::Abs(expression),
					"ceil" => Function::Ceil(expression),
					"floor" => Function::Floor(expression),
					"round" => Function::Round(expression),
					_ => return Err(ParseError::Unknown),
				};
				self.skip_whitespace();
				if let Err(parse_error) = self.match_current_to_punctuation_skip_whitespace(")") {
					self.current_index = start_index;
					return Err(parse_error);
				}
				return Ok(function)
			}
		}
		Err(ParseError::DoesNotMatch)
	}

	fn parse_number(&mut self) -> Result<Number, ParseError> {
		let start_index = self.current_index;
		self.skip_whitespace();
		match self.parse_float() {
			Ok(float) => return Ok(Number::Float(float)),
			Err(_parse_error) => self.current_index = start_index,
		};
		match self.parse_integer() {
			Ok(int) => Ok(Number::Integer(int)),
			Err(_parse_error) => Err(ParseError::DoesNotMatch),
		}
	}
	fn parse_float(&mut self) -> Result<Float, ParseError> {
		if let Lexeme::Number(integer_token) = self.current()?.clone() {
			let start_index = self.current_index;
			self.step_lexemes();
			if let Err(parse_error) = self.match_current_to_punctuation(".") {
				self.current_index = start_index;
				return Err(parse_error);
			};
			if let Lexeme::Number(fraction_token) = self.current()?.clone() {
				self.step_lexemes();
				return Ok(Float::new(
					format!("{}.{}", integer_token.source(), fraction_token.source())
						.parse()
						.unwrap(),
				))
			}
		}
		Err(ParseError::DoesNotMatch)
	}
	fn parse_integer(&mut self) -> Result<Integer, ParseError> {
		if let Lexeme::Number(integer_token) = self.current()?.clone() {
			self.step_lexemes();
			return Ok(Integer::new(integer_token.source().parse().unwrap()));
		}
		Err(ParseError::DoesNotMatch)
	}

	fn parse_roll_query(&mut self) -> Result<RollQuery, ParseError> {
		self.skip_whitespace();
		let start_index = self.current_index;
		self.match_current_to_punctuation("?")?;
		if let Err(parse_error) = self.match_current_to_punctuation("{") {
			self.current_index = start_index;
			return Err(parse_error);
		}
		let mut roll_query = RollQuery::new();
		let mut prompt_complete = false;
		loop {
			match self.match_current_to_punctuation_skip_whitespace("|") {
				Ok(_token) => prompt_complete = true,
				Err(_parse_error) => {
					if !prompt_complete {
						roll_query
							.prompt
							.push_str(&self.current()?.token().source());
						self.step_lexemes();
					} else {
						roll_query
							.default
							.push_str(&self.current()?.token().source());
						self.step_lexemes();
					}
				}
			}
			if self.match_current_to_punctuation_skip_whitespace("}").is_ok() {
				break
			}
		}
		Ok(roll_query)
	}

	fn parse_dice(&mut self) -> Result<Dice, ParseError> {
		let start_index = self.current_index;
		match self.parse_normal() {
			Ok(normal) => {
				return Ok(Dice::Normal(
					normal,
					self.parse_modifiers(),
					self.parse_comment(),
				));
			}
			Err(_parse_error) => self.current_index = start_index,
		};
		match self.parse_fate() {
			Ok(fate) => {
				return Ok(Dice::Fate(
					fate,
					self.parse_modifiers(),
					self.parse_comment(),
				));
			}
			Err(_parse_error) => self.current_index = start_index,
		};
		match self.parse_computed() {
			Ok(computed) => {
				return Ok(Dice::Computed(
					computed,
					self.parse_modifiers(),
					self.parse_comment(),
				));
			}
			Err(_parse_error) => self.current_index = start_index,
		}
		Err(ParseError::DoesNotMatch)
	}
	fn parse_normal(&mut self) -> Result<Normal, ParseError> {
		let start_index = self.current_index;
		let count = match self.parse_integer() {
			Ok(integer) => integer,
			Err(_parse_error) => {
				self.current_index = start_index;
				Integer::new(1)
			}
		};
		if let Err(parse_error) = self.match_current_to_literal("d") {
			self.current_index = start_index;
			return Err(parse_error);
		};
		match self.parse_integer() {
			Ok(sides) => Ok(Normal { count, sides }),
			Err(parse_error) => {
				self.current_index = start_index;
				Err(parse_error)
			}
		}
	}
	fn parse_fate(&mut self) -> Result<Fate, ParseError> {
		let start_index = self.current_index;
		let count = self.parse_integer()?;
		match self.match_current_to_literal("dF") {
			Ok(_token) => Ok(Fate { count }),
			Err(parse_error) => {
				self.current_index = start_index;
				Err(parse_error)
			}
		}
	}
	// <computed> ::=
	//       [(<integer> | <roll_query>)] "d" "(" <expression> ")"
	//     | "(" <expression> ")" "d" (<integer> | <roll_query>)
	//     | ["(" <expression> ")"] "d" "(" <expression> ")"
	fn parse_computed(&mut self) -> Result<Computed, ParseError> {
		let count = self.parse_computed_helper()?;
		self.match_current_to_literal("d")?;
		let sides = self.parse_computed_helper()?;
		Ok(Computed {
			count: Box::new(count),
			sides: Box::new(sides),
		})
	}
	fn parse_computed_helper(&mut self) -> Result<Expression, ParseError> {
		match self.parse_integer() {
			Ok(int) => Ok(int.as_expression()),
			Err(_parse_error) => match self.parse_roll_query() {
				Ok(query) => Ok(query.as_expression()),
				Err(_parse_error) => {
					self.match_current_to_punctuation_skip_whitespace("(")?;
					let expression = self.parse_expression()?;
					self.match_current_to_punctuation(")")?;
					Ok(expression)
				}
			},
		}
	}
	fn parse_modifiers(&mut self) -> Modifiers {
		let mut modifiers = Modifiers::new();

		modifiers.expanding = self.parse_expanding().ok();
		loop {
			let mut found_one = false;
			let start_index = self.current_index;
			match self.parse_reroll() {
				Ok(reroll_modifier) => {
					modifiers.reroll_modifiers.push(reroll_modifier);
					found_one = true;
				}
				Err(_parse_error) => self.current_index = start_index,
			};
			let start_index = self.current_index;
			match self.parse_successes() {
				Ok(successes_modifier) => {
					modifiers.post_modifiers.push(successes_modifier);
					found_one = true;
				}
				Err(_parse_error) => self.current_index = start_index,
			};
			let start_index = self.current_index;
			match self.parse_cirtical() {
				Ok(critical_modifier) => {
					modifiers.post_modifiers.push(critical_modifier);
					found_one = true;
				}
				Err(_parse_error) => self.current_index = start_index,
			};
			let start_index = self.current_index;
			match self.parse_high_low() {
				Ok(critical_modifier) => {
					modifiers.post_modifiers.push(critical_modifier);
					found_one = true;
				}
				Err(_parse_error) => self.current_index = start_index,
			};
			if !found_one {
				break;
			}
		}
		modifiers
	}
	fn parse_comparison(&mut self) -> Result<Comparison, ParseError> {
		let comparison = match self.current()? {
			Lexeme::Comparison(token) => match token.source() {
				"<" => Ok(Comparison::LessThan),
				">" => Ok(Comparison::GreaterThan),
				"<=" => Ok(Comparison::LessThanEqual),
				">=" => Ok(Comparison::GreaterThanEqual),
				"=" => Ok(Comparison::Equal),
				_ => return Err(ParseError::DoesNotMatch),
			},
			_ => return Err(ParseError::DoesNotMatch),
		};
		self.step_lexemes();
		comparison
	}
	fn parse_comparison_and_integer(
		&mut self,
	) -> Result<(Comparison, Option<Integer>), ParseError> {
		let start_index = self.current_index;
		match self.parse_comparison() {
			Ok(comparison) => match self.parse_integer() {
				Ok(integer) => Ok((comparison, Some(integer))),
				Err(_parse_error) => {
					self.current_index = start_index;
					Err(ParseError::ExpectedInteger)
				}
			},
			Err(_parse_error) => Ok((Comparison::Equal, self.parse_integer().ok())),
		}
	}
	fn parse_comparison_and_require_integer(
		&mut self,
	) -> Result<(Comparison, Integer), ParseError> {
		let start_index = self.current_index;
		let comparison = match self.parse_comparison() {
			Ok(comparison) => comparison,
			Err(_parse_error) => Comparison::Equal,
		};
		match self.parse_integer() {
			Ok(integer) => Ok((comparison, integer)),
			Err(_parse_error) => {
				self.current_index = start_index;
				Err(ParseError::ExpectedInteger)
			}
		}
	}

	fn parse_expanding(&mut self) -> Result<Expanding, ParseError> {
		let start_index = self.current_index;
		match self.parse_exploding() {
			Ok(exploding) => return Ok(Expanding::Exploding(exploding)),
			Err(_parse_error) => self.current_index = start_index,
		}
		match self.parse_compounding() {
			Ok(exploding) => return Ok(Expanding::Compounding(exploding)),
			Err(_parse_error) => self.current_index = start_index,
		}
		Ok(Expanding::Penetrating(self.parse_penetrating()?))
	}

	fn parse_exploding(&mut self) -> Result<Exploding, ParseError> {
		let start_index = self.current_index;
		if let Err(parse_error) = self.match_current_to_operator("!") {
			self.current_index = start_index;
			return Err(parse_error);
		}
		match self.parse_comparison_and_require_integer() {
			Ok((comparison, integer)) => Ok(Exploding::new(comparison, Some(integer))),
			Err(_) => Ok(Exploding::new(Comparison::Equal, None)),
		}
	}
	fn parse_compounding(&mut self) -> Result<Compounding, ParseError> {
		let start_index = self.current_index;
		if let Err(parse_error) = self.match_current_to_operator("!!") {
			self.current_index = start_index;
			return Err(parse_error);
		}
		match self.parse_comparison_and_require_integer() {
			Ok((comparison, integer)) => Ok(Penetrating::new(comparison, Some(integer))),
			Err(_) => Ok(Compounding::new(Comparison::Equal, None)),
		}
	}
	fn parse_penetrating(&mut self) -> Result<Penetrating, ParseError> {
		let start_index = self.current_index;
		if let Err(parse_error) = self.match_current_to_operator("!p") {
			self.current_index = start_index;
			return Err(parse_error);
		}
		match self.parse_comparison_and_require_integer() {
			Ok((comparison, integer)) => Ok(Exploding::new(comparison, Some(integer))),
			Err(_) => Ok(Exploding::new(Comparison::Equal, None)),
		}
	}
	fn parse_reroll(&mut self) -> Result<Reroll, ParseError> {
		let start_index = self.current_index;
		self.match_current_to_literal("r")?;
		match self.parse_comparison_and_require_integer() {
			Ok((comparison, integer)) => Ok(Reroll::new(comparison, Some(integer))),
			Err(parse_error) => {
				self.current_index = start_index;
				Err(parse_error)
			}
		}
	}
	fn parse_high_low(&mut self) -> Result<PostModifier, ParseError> {
		if let Lexeme::Literal(token) = self.current()?.clone() {
			if let "dh" | "k" | "kh" | "d" | "dl" | "kl" = token.source() {
				let start_index = self.current_index;
				self.step_lexemes_skip_whitespace();
				let count = match self.parse_integer() {
					Ok(int) => int,
					Err(_parse_error) => {
						self.current_index = start_index;
						return Err(ParseError::ExpectedInteger);
					}
				};
				return match token.source() {
					"dh" => Ok(PostModifier::DropHighest(count)),
					"k" | "kh" => Ok(PostModifier::KeepHighest(count)),
					"d" | "dl" => Ok(PostModifier::DropLowest(count)),
					"kl" => Ok(PostModifier::KeepLowest(count)),
					_ => Err(ParseError::Unknown),
				};
			}
		}
		Err(ParseError::DoesNotMatch)
	}
	fn parse_successes(&mut self) -> Result<PostModifier, ParseError> {
		let comparison = self.parse_comparison()?;
		let integer = self.parse_integer()?;
		Ok(PostModifier::Success(comparison, integer))
	}
	fn parse_cirtical(&mut self) -> Result<PostModifier, ParseError> {
		if let Lexeme::Literal(token) =  self.current()?.clone() {
			if let "cs" | "cf" = token.source() {
				let start_index = self.current_index;
				self.step_lexemes_skip_whitespace();
				let (comparison, integer) = match self.parse_comparison_and_require_integer() {
					Ok((comparison, integer)) => (comparison, integer),
					Err(parse_error) => {
						self.current_index = start_index;
						return Err(parse_error);
					}
				};
				return match token.source() {
					"cs" => Ok(PostModifier::CriticalSuccess(comparison, integer)),
					"cf" => Ok(PostModifier::CriticalFailure(comparison, integer)),
					_ => Err(ParseError::Unknown),
				};
			}
		}
		Err(ParseError::DoesNotMatch)
	}

	fn step_lexemes(&mut self) {
		self.current_index += 1;
	}
	fn step_lexemes_skip_whitespace(&mut self) {
		self.current_index += 1;
		if let Ok(lexeme) = self.current() {
			if let Lexeme::Whitespace(_token) = lexeme {
				self.step_lexemes_skip_whitespace();
			}
		}
	}
	fn skip_whitespace(&mut self) {
		if let Ok(lexeme) = self.current() {
			if let Lexeme::Whitespace(_token) = lexeme {
				self.current_index += 1;
				self.skip_whitespace();
			}
		}
	}

	fn current(&self) -> Result<&Lexeme, ParseError> {
		match self.current_as_option() {
			Some(lexeme) => Ok(lexeme),
			None => Err(ParseError::OutOfBounds),
		}
	}
	fn next(&self) -> Result<&Lexeme, ParseError> {
		match self.next_as_option() {
			Some(lexeme) => Ok(lexeme),
			None => Err(ParseError::OutOfBounds),
		}
	}
	fn current_as_option(&self) -> Option<&Lexeme> {
		self.lexemes.get(self.current_index)
	}
	fn next_as_option(&self) -> Option<&Lexeme> {
		self.lexemes.get(self.current_index + 1)
	}

	fn match_current_to_punctuation(&mut self, punctuation: &str) -> Result<Token, ParseError> {
		if let Lexeme::Punctuation(token) = self.current()?.clone() {
			if token.source() == punctuation {
				self.step_lexemes();
				return Ok(token);
			}
		}
		Err(ParseError::DoesNotMatch)
	}
	fn match_current_to_punctuation_skip_whitespace(
		&mut self,
		punctuation: &str,
	) -> Result<Token, ParseError> {
		if let Lexeme::Punctuation(token) = self.current()?.clone() {
			if token.source() == punctuation {
				self.step_lexemes_skip_whitespace();
				return Ok(token);
			}
		}
		Err(ParseError::DoesNotMatch)
	}
	fn match_current_to_literal(&mut self, literal: &str) -> Result<Token, ParseError> {
		if let Lexeme::Literal(token) = self.current()?.clone() {
			if token.source() == literal {
				self.step_lexemes_skip_whitespace();
				return Ok(token);
			}
		}
		Err(ParseError::DoesNotMatch)
	}
	fn match_current_to_operator(&mut self, operator: &str) -> Result<Token, ParseError> {
		if let Lexeme::Operator(token) = self.current()?.clone() {
			if token.source() == operator {
				self.step_lexemes_skip_whitespace();
				return Ok(token);
			}
		}
		Err(ParseError::DoesNotMatch)
	}

	fn is_roll(&self) -> bool {
		let current = self.current_as_option();
		let slash = current.is_some()
			&& match current.unwrap() {
				Lexeme::Operator(token) => token.source() == "/",
				_ => false,
			};
		let next = self.next_as_option();
		let roll = next.is_some()
			&& match next.unwrap() {
				Lexeme::Literal(token) => token.source() == "roll" || token.source() == "r",
				_ => false,
			};
		slash && roll
	}
	fn is_inline_roll(&self) -> bool {
		let is_open_bracket = |lexeme: &Option<&Lexeme>| -> bool {
			lexeme.is_some()
				&& match lexeme.unwrap() {
					Lexeme::Punctuation(token) => token.source() == "[",
					_ => false,
				}
		};
		let current = is_open_bracket(&self.current_as_option());
		let next = is_open_bracket(&self.next_as_option());
		current && next
	}
}
