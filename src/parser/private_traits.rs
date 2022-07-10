// File: parser/private_traits.rs

use super::{error::ParseError, state::State, Parser};
use crate::ast::{number::*, *};
use crate::lexer::{
	lexeme::Lexeme,
	token::{Token, TokenT},
};

pub(super) trait ParserPrivateT {
	fn parse_start(&mut self);

	fn parse_string_literal(&mut self) -> String;

	fn template_parse_comment(&mut self) -> Option<String>;
	fn parse_comment(&mut self) -> Option<String>;
	fn parse_tooltip(&mut self) -> Option<String>;

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

	fn parse_modifiers(&mut self) -> Result<Modifiers, ParseError>;
	fn parse_comparison(&mut self) -> Result<Comparison, ParseError>;
	fn parse_comparison_and_integer(&mut self)
		-> Result<(Comparison, Option<Integer>), ParseError>;
	fn parse_comparison_and_require_integer(&mut self)
		-> Result<(Comparison, Integer), ParseError>;

	fn parse_exploding(&mut self) -> Result<Reroll, ParseError>;
	fn parse_compounding(&mut self) -> Result<Reroll, ParseError>;
	fn parse_penetrating(&mut self) -> Result<Reroll, ParseError>;

	fn parse_reroll(&mut self) -> Result<Reroll, ParseError>;
	fn parse_drop_keep(&mut self) -> Result<DropKeep, ParseError>;
	fn parse_successes(&mut self) -> Result<Successes, ParseError>;
	fn parse_cirtical(&mut self) -> Result<Successes, ParseError>;
	fn parse_sort(&mut self) -> Result<Sort, ParseError>;

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
	fn match_current_to_keyword(&mut self, keyword: &str) -> Result<Token, ParseError>;
	fn match_current_to_operator(&mut self, operator: &str) -> Result<Token, ParseError>;

	fn is_roll(&self) -> bool;
	fn is_inline_roll(&self) -> bool;
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
			} else if self.current().unwrap().token().source() == "#" { // what is this for?
				break;
			} else {
				literal.push_str(self.current().unwrap().source());
				self.step_lexemes();
			}
		}
		literal
	}
	fn template_parse_comment(&mut self) -> Option<String> {
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

	fn parse_comment(&mut self) -> Option<String> {
		let start_index = self.current_index;
		self.match_current_to_punctuation("[").ok()?;
		if self.match_current_to_punctuation("[").is_ok()
			|| self.match_current_to_punctuation("?").is_ok()
		{
			self.current_index = start_index;
			return None;
		}

		self.template_parse_comment()
	}
	fn parse_tooltip(&mut self) -> Option<String> {
		let start_index = self.current_index;
		self.match_current_to_punctuation("[").ok()?;
		if self.match_current_to_punctuation("[").is_ok()
			|| self.match_current_to_punctuation("?").is_err()
		{
			self.current_index = start_index;
			return None;
		}

		self.template_parse_comment()
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
							macro_name.push_str(self.current()?.token().source());
							self.current_index += 1;
						}
					}
				}
				return Ok(Macro { name: macro_name });
			}
			Err(_parse_error) => match self.current()? {
				Lexeme::Literal(token) | Lexeme::Keyword(token) => {
					let token = token.clone();
					self.step_lexemes();
					return Ok(Macro {
						name: token.source().to_string(),
					});
				}
				_ => (),
			},
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
			let is_add = if let Ok(Lexeme::Operator(token)) = self.current() {
				match token.source() {
					"+" => true,
					"-" => false,
					_ => break,
				}
			} else {
				break;
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
			let is_multiply = if let Ok(Lexeme::Operator(token)) = self.current() {
				match token.source() {
					"*" => true,
					"/" => false,
					_ => break,
				}
			} else {
				break;
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
		if let Ok(Lexeme::Operator(token)) = self.current() {
			if let "**" | "^" = token.source() {
				let start_index = self.current_index;
				self.step_lexemes();
				match self.parse_power() {
					Ok(power) => return Ok(Power::Pow(lhs, Box::new(power))),
					Err(_parse_error) => self.current_index = start_index,
				};
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
		let check_tooltip = |tip_1: Option<String>, tip_2: Option<String>| match (tip_1, tip_2) {
			(Some(_), Some(unexepected)) => Err(ParseError::UnexpectedTooltip(unexepected)),
			(Some(t), None) | (None, Some(t)) => Ok(Some(t)),
			(_, _) => Ok(None),
		};

		let start_index = self.current_index;
		let tooltip = self.parse_tooltip();
		let after_tip_index = self.current_index;
		match self.parse_dice() {
			Ok(dice) => {
				let tooltip = check_tooltip(tooltip, self.parse_tooltip())?;
				return Ok(Atom::Dice(dice, tooltip));
			}
			Err(parse_error) => match parse_error {
				ParseError::MultipleTypesOfExpandingModifiersNotSupported
				| ParseError::MultipleDropKeepModifiersNotSupported
				| ParseError::MultipleSortModifiersNotSupported => return Err(parse_error),
				_ => (),
			},
		};
		self.current_index = after_tip_index;

		if let Ok(num) = self.parse_number() {
			let tooltip = check_tooltip(tooltip, self.parse_tooltip())?;
			return Ok(Atom::Number(num, tooltip));
		}
		self.current_index = start_index;

		if let Some(tip) = tooltip {
			return Err(ParseError::UnexpectedTooltip(tip));
		}

		if let Ok(query) = self.parse_roll_query() {
			return Ok(Atom::RollQuery(query));
		}
		self.current_index = start_index;

		if let Ok(function) = self.parse_function() {
			return Ok(Atom::Function(function));
		};
		self.current_index = start_index;

		if let Ok(nested_macro) = self.parse_macro() {
			return Ok(Atom::Macro(nested_macro));
		};
		self.current_index = start_index;

		if self.is_inline_roll() {
			self.step_lexemes();
			self.step_lexemes();
			if let Ok(expression) = self.parse_expression() {
				if self.match_current_to_punctuation("]").is_ok()
					&& self.match_current_to_punctuation("]").is_ok()
				{
					return Ok(Atom::InlineRoll(Box::new(expression)));
				} else {
					self.current_index = start_index;
				}
			} else {
				self.current_index = start_index;
			}
		}

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
		if let Lexeme::Keyword(token) = self.current()? {
			if let "abs" | "ceil" | "floor" | "round" | "round_half_down" = token.source() {
				let start_index = self.current_index;
				let token = token.clone();
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
					"round_half_down" => Function::RoundHalfDown(expression),
					_ => return Err(ParseError::Unknown),
				};
				self.skip_whitespace();
				if let Err(parse_error) = self.match_current_to_punctuation_skip_whitespace(")") {
					self.current_index = start_index;
					return Err(parse_error);
				}
				return Ok(function);
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
		if let Lexeme::Number(integer_token) = self.current()? {
			let start_index = self.current_index;
			let integer_token = integer_token.clone();
			self.step_lexemes();
			if let Err(_parse_error) = self.match_current_to_punctuation(".") {
				self.current_index = start_index;
				return Err(ParseError::ExpectedPunctuation(String::from(".")));
			};
			if let Lexeme::Number(decimal_token) = self.current()? {
				let decimal_token = decimal_token.clone();
				self.step_lexemes();
				return Ok(
					format!("{}.{}", integer_token.source(), decimal_token.source())
						.parse()
						.unwrap(),
				);
			}
		}
		Err(ParseError::DoesNotMatch)
	}
	fn parse_integer(&mut self) -> Result<Integer, ParseError> {
		if let Lexeme::Number(token) = self.current()? {
			let token = token.clone();
			self.step_lexemes();
			return Ok(token.source().parse().unwrap());
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
							.push_str(self.current()?.token().source());
						self.step_lexemes();
					} else {
						roll_query
							.default
							.push_str(self.current()?.token().source());
						self.step_lexemes();
					}
				}
			}
			if self
				.match_current_to_punctuation_skip_whitespace("}")
				.is_ok()
			{
				break;
			}
		}
		Ok(roll_query)
	}

	fn parse_dice(&mut self) -> Result<Dice, ParseError> {
		let start_index = self.current_index;
		match self.parse_normal() {
			Ok(normal) => {
				return Ok(Dice::Normal(normal, self.parse_modifiers()?));
			}
			Err(_parse_error) => self.current_index = start_index,
		};
		match self.parse_fate() {
			Ok(fate) => {
				return Ok(Dice::Fate(fate, self.parse_modifiers()?));
			}
			Err(_parse_error) => self.current_index = start_index,
		};
		match self.parse_computed() {
			Ok(computed) => {
				return Ok(Dice::Computed(computed, self.parse_modifiers()?));
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
				1
			}
		};
		if let Err(parse_error) = self.match_current_to_keyword("d") {
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
		match self.match_current_to_keyword("dF") {
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
		self.match_current_to_keyword("d")?;
		let sides = self.parse_computed_helper()?;
		Ok(Computed {
			count: Box::new(count),
			sides: Box::new(sides),
		})
	}
	fn parse_computed_helper(&mut self) -> Result<Expression, ParseError> {
		match self.parse_integer() {
			Ok(int) => Ok(integer_as_expression(int)),
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
	fn parse_modifiers(&mut self) -> Result<Modifiers, ParseError> {
		let mut modifiers = Modifiers::default();
		let mut expanding = None;
		loop {
			let mut found_one = false;
			let start_index = self.current_index;
			match self.parse_exploding() {
				Ok(exploding) => {
					if expanding.is_none() {
						expanding = Some(Expanding::Exploding(vec![exploding]));
					} else if let Some(Expanding::Exploding(exploding_points)) = expanding.as_mut()
					{
						exploding_points.push(exploding);
					} else {
						return Err(ParseError::MultipleTypesOfExpandingModifiersNotSupported);
					}
					found_one = true;
				}
				Err(_parse_error) => self.current_index = start_index,
			}
			let start_index = self.current_index;
			match self.parse_penetrating() {
				Ok(penetrating) => {
					if expanding.is_none() {
						expanding = Some(Expanding::Penetrating(vec![penetrating]));
					} else if let Some(Expanding::Penetrating(penetrating_points)) =
						expanding.as_mut()
					{
						penetrating_points.push(penetrating);
					} else {
						return Err(ParseError::MultipleTypesOfExpandingModifiersNotSupported);
					}
					found_one = true;
				}
				Err(_parse_error) => self.current_index = start_index,
			}
			let start_index = self.current_index;
			match self.parse_compounding() {
				Ok(compounding) => {
					if expanding.is_none() {
						expanding = Some(Expanding::Compounding(vec![compounding]));
					} else if let Some(Expanding::Compounding(compounding_points)) =
						expanding.as_mut()
					{
						compounding_points.push(compounding);
					} else {
						return Err(ParseError::MultipleTypesOfExpandingModifiersNotSupported);
					}
					found_one = true;
				}
				Err(_parse_error) => self.current_index = start_index,
			}
			let start_index = self.current_index;

			match self.parse_reroll() {
				Ok(reroll_modifier) => {
					modifiers.reroll.push(reroll_modifier);
					found_one = true;
				}
				Err(_parse_error) => self.current_index = start_index,
			};
			let start_index = self.current_index;
			match self.parse_successes() {
				Ok(successes_modifier) => {
					modifiers.successes.push(successes_modifier);
					found_one = true;
				}
				Err(_parse_error) => self.current_index = start_index,
			};
			let start_index = self.current_index;
			match self.parse_cirtical() {
				Ok(critical_modifier) => {
					modifiers.successes.push(critical_modifier);
					found_one = true;
				}
				Err(_parse_error) => self.current_index = start_index,
			};
			let start_index = self.current_index;
			match self.parse_drop_keep() {
				Ok(drop_keep) => {
					if modifiers.drop_keep.is_some() {
						return Err(ParseError::MultipleDropKeepModifiersNotSupported);
					}
					modifiers.drop_keep = Some(drop_keep);
					found_one = true;
				}
				Err(_parse_error) => self.current_index = start_index,
			};
			let start_index = self.current_index;
			match self.parse_sort() {
				Ok(sort) => {
					if modifiers.sort.is_some() {
						return Err(ParseError::MultipleSortModifiersNotSupported);
					}
					modifiers.sort = Some(sort);
					println!("Found sourt modifier {:?}", sort);
					found_one = true;
				}
				Err(_parse_error) => self.current_index = start_index,
			}
			if !found_one {
				break;
			}
		}
		modifiers.expanding = expanding;
		Ok(modifiers)
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

	fn parse_exploding(&mut self) -> Result<Reroll, ParseError> {
		let start_index = self.current_index;
		if let Err(parse_error) = self.match_current_to_operator("!") {
			self.current_index = start_index;
			return Err(parse_error);
		}
		match self.parse_comparison_and_require_integer() {
			Ok((comparison, integer)) => Ok(Reroll::new(comparison, Some(integer))),
			Err(_) => Ok(Reroll::new(Comparison::Equal, None)),
		}
	}
	fn parse_compounding(&mut self) -> Result<Reroll, ParseError> {
		let start_index = self.current_index;
		if let Err(parse_error) = self.match_current_to_operator("!!") {
			self.current_index = start_index;
			return Err(parse_error);
		}
		match self.parse_comparison_and_require_integer() {
			Ok((comparison, integer)) => Ok(Reroll::new(comparison, Some(integer))),
			Err(_) => Ok(Reroll::new(Comparison::Equal, None)),
		}
	}
	fn parse_penetrating(&mut self) -> Result<Reroll, ParseError> {
		let start_index = self.current_index;
		if let Err(parse_error) = self.match_current_to_operator("!p") {
			self.current_index = start_index;
			return Err(parse_error);
		}
		match self.parse_comparison_and_require_integer() {
			Ok((comparison, integer)) => Ok(Reroll::new(comparison, Some(integer))),
			Err(_) => Ok(Reroll::new(Comparison::Equal, None)),
		}
	}
	fn parse_reroll(&mut self) -> Result<Reroll, ParseError> {
		let start_index = self.current_index;
		self.match_current_to_keyword("r")?;
		match self.parse_comparison_and_require_integer() {
			Ok((comparison, integer)) => Ok(Reroll::new(comparison, Some(integer))),
			Err(parse_error) => {
				self.current_index = start_index;
				Err(parse_error)
			}
		}
	}
	fn parse_drop_keep(&mut self) -> Result<DropKeep, ParseError> {
		if let Lexeme::Keyword(token) = self.current()? {
			if let "dh" | "k" | "kh" | "d" | "dl" | "kl" = token.source() {
				let token = token.clone();
				self.step_lexemes_skip_whitespace();
				let count = match self.parse_integer() {
					Ok(int) => int,
					Err(parse_error) => return Err(parse_error),
				};
				return match token.source() {
					"dh" => Ok(DropKeep::DropHighest(count)),
					"k" | "kh" => Ok(DropKeep::KeepHighest(count)),
					"d" | "dl" => Ok(DropKeep::DropLowest(count)),
					"kl" => Ok(DropKeep::KeepLowest(count)),
					_ => Err(ParseError::Unknown),
				};
			}
		}
		Err(ParseError::DoesNotMatch)
	}
	fn parse_successes(&mut self) -> Result<Successes, ParseError> {
		let comparison = self.parse_comparison()?;
		let integer = self.parse_integer()?;
		Ok(Successes::Success(comparison, integer))
	}
	fn parse_cirtical(&mut self) -> Result<Successes, ParseError> {
		if let Lexeme::Keyword(token) = self.current()? {
			if let "cs" | "cf" = token.source() {
				let start_index = self.current_index;
				let token = token.clone();
				self.step_lexemes_skip_whitespace();
				let (comparison, integer) = match self.parse_comparison_and_require_integer() {
					Ok((comparison, integer)) => (comparison, integer),
					Err(parse_error) => {
						self.current_index = start_index;
						return Err(parse_error);
					}
				};
				return match token.source() {
					"cs" => Ok(Successes::CriticalSuccess(comparison, integer)),
					"cf" => Ok(Successes::CriticalFailure(comparison, integer)),
					_ => Err(ParseError::Unknown),
				};
			}
		}
		Err(ParseError::DoesNotMatch)
	}
	fn parse_sort(&mut self) -> Result<Sort, ParseError> {
		if let Lexeme::Keyword(token) = self.current()? {
			match token.source() {
				"s" | "sa" => {
					self.step_lexemes();
					return Ok(Sort::Ascending);
				}
				"sd" => {
					self.step_lexemes();
					return Ok(Sort::Decending);
				}
				_ => (),
			}
		}
		Err(ParseError::DoesNotMatch)
	}

	fn step_lexemes(&mut self) {
		self.current_index += 1;
	}
	fn step_lexemes_skip_whitespace(&mut self) {
		self.current_index += 1;
		if let Ok(Lexeme::Whitespace(_)) = self.current() {
			self.step_lexemes_skip_whitespace();
		}
	}
	fn skip_whitespace(&mut self) {
		if let Ok(Lexeme::Whitespace(_token)) = self.current() {
			self.current_index += 1;
			self.skip_whitespace();
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
		if let Lexeme::Punctuation(token) = self.current()? {
			if token.source() == punctuation {
				let token = token.clone();
				self.step_lexemes();
				return Ok(token);
			}
		}
		Err(ParseError::ExpectedPunctuation(punctuation.to_owned()))
	}
	fn match_current_to_punctuation_skip_whitespace(
		&mut self,
		punctuation: &str,
	) -> Result<Token, ParseError> {
		if let Lexeme::Punctuation(token) = self.current()? {
			if token.source() == punctuation {
				let token = token.clone();
				self.step_lexemes_skip_whitespace();
				return Ok(token);
			}
		}
		Err(ParseError::ExpectedPunctuation(punctuation.to_owned()))
	}
	fn match_current_to_literal(&mut self, literal: &str) -> Result<Token, ParseError> {
		if let Lexeme::Literal(token) = self.current()? {
			if token.source() == literal {
				let token = token.clone();
				self.step_lexemes_skip_whitespace();
				return Ok(token);
			}
		}
		Err(ParseError::ExpectedLiteral(literal.to_owned()))
	}
	fn match_current_to_keyword(&mut self, keyword: &str) -> Result<Token, ParseError> {
		if let Lexeme::Keyword(token) = self.current()? {
			if token.source() == keyword {
				let token = token.clone();
				self.step_lexemes_skip_whitespace();
				return Ok(token);
			}
		}
		Err(ParseError::ExpectedKeyword(keyword.to_owned()))
	}
	fn match_current_to_operator(&mut self, operator: &str) -> Result<Token, ParseError> {
		if let Lexeme::Operator(token) = self.current()? {
			if token.source() == operator {
				let token = token.clone();
				self.step_lexemes_skip_whitespace();
				return Ok(token);
			}
		}
		Err(ParseError::ExpectedOperator(operator.to_owned()))
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
				Lexeme::Keyword(token) => token.source() == "roll" || token.source() == "r",
				_ => false,
			};
		slash && roll
	}
	fn is_inline_roll(&self) -> bool {
		let is_open_bracket = |lexeme: &Option<&Lexeme>| -> bool {
			if let Some(Lexeme::Punctuation(token)) = lexeme {
				token.source() == "["
			} else {
				false
			}
		};
		let current = is_open_bracket(&self.current_as_option());
		let next = is_open_bracket(&self.next_as_option());
		current && next
	}
}
