// File: parser/private_traits.rs

use super::{
	error::{
		Expecting,
		ParseError,
	},
	state::State,
	Parser,
};
use crate::ast::{
	number::*,
	*,
};
use crate::lexer::{
	keywords::Keyword,
	lexeme::Lexeme,
	token::TokenT,
};

pub(super) trait ParserPrivateT {
	fn parse_start(&mut self);

	fn parse_string_literal(&mut self) -> String;

	fn template_parse_comment(&mut self) -> Option<String>;
	fn parse_comment(&mut self) -> Option<String>;
	fn parse_tooltip(&mut self) -> Option<String>;

	fn parse_macro(&mut self) -> Result<Macro, ParseError>;

	fn parse_roll(&mut self) -> Result<Roll, ParseError>;
	fn parse_inline_roll(&mut self) -> Result<Expression, ParseError>;

	fn parse_expression(&mut self) -> Result<Expression, ParseError>;
	fn parse_mul_div(&mut self) -> Result<MulDiv, ParseError>;
	fn parse_power(&mut self) -> Result<Power, ParseError>;
	fn parse_unary(&mut self) -> Result<Unary, ParseError>;
	fn parse_atom(&mut self) -> Result<Atom, ParseError>;

	fn parse_parentheses_expression(&mut self) -> Result<Expression, ParseError>;
	fn parse_function(&mut self) -> Result<Function, ParseError>;

	fn parse_number(&mut self) -> Result<Number, ParseError>;
	fn parse_integer(&mut self) -> Result<Integer, ParseError>;

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
	fn parse_reroll_once(&mut self) -> Result<Reroll, ParseError>;

	fn parse_drop_keep(&mut self) -> Result<DropKeep, ParseError>;
	fn parse_successes(&mut self) -> Result<Successes, ParseError>;
	fn parse_cirtical(&mut self) -> Result<Successes, ParseError>;
	fn parse_sort(&mut self) -> Result<Sort, ParseError>;

	fn step_lexemes(&mut self);
	fn step_lexemes_skip_whitespace(&mut self);
	fn skip_whitespace(&mut self);

	fn current(&mut self) -> Result<&Lexeme, ParseError>;
	fn current_as_option(&mut self) -> Option<&Lexeme>;

	fn match_current_to_punctuation(&mut self, punctuation: &str) -> Result<(), ParseError>;
	fn match_current_to_keyword(&mut self, keyword: Keyword) -> Result<(), ParseError>;
	fn match_current_to_one_of_keywords(
		&mut self,
		keyword: &[Keyword],
	) -> Result<Keyword, ParseError>;
	fn match_current_to_operator(&mut self, operator: &str) -> Result<(), ParseError>;

	fn is_roll(&mut self) -> bool;
	fn is_inline_roll(&mut self) -> bool;
}

impl<'a> ParserPrivateT for Parser<'a> {
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
			} else if self.current().unwrap().source() == "#" {
				// start of macro, handled by caller
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
				Ok(_token) => break,
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

		if let Err(err) = self.match_current_to_punctuation("#") {
			self.current_index = start_index;
			return Err(err);
		}

		let start_byte = {
			self.current_index -= 1;
			let token = self.current_as_option().unwrap().token();
			token.start()
		};
		self.step_lexemes();

		match self.match_current_to_punctuation("{") {
			Ok(_token) => {
				let mut macro_name = String::new();
				let mut end = 0;
				loop {
					match self.match_current_to_punctuation("}") {
						Ok(_token) => break,
						Err(_parse_error) => {
							macro_name.push_str(self.current()?.token().source());
							self.step_lexemes();
							end = self.current().unwrap().token().end();
						}
					}
				}

				Ok(Macro {
					start: start_byte,
					end,
					name: macro_name,
				})
			}
			Err(_parse_error) => {
				let mut end = 0;

				let mut macro_name = String::new();
				loop {
					if let Ok(Lexeme::Literal(token)) = self.current() {
						macro_name.push_str(token.source());
						end = token.end();
					} else if let Ok(Lexeme::Keyword(token, _)) = self.current() {
						macro_name.push_str(token.source());
						end = token.end();
					} else {
						break;
					}
					self.step_lexemes();
				}

				Ok(Macro {
					start: start_byte,
					end,
					name: macro_name,
				})
			}
		}
	}
	fn parse_roll(&mut self) -> Result<Roll, ParseError> {
		if self.is_roll() {
			let start = self.current_index;

			self.step_lexemes();
			self.step_lexemes();

			let expression = self.parse_expression()?;

			let index_after_expression = self.current_index;
			if self.match_current_to_punctuation("\\").is_err() {
				self.skip_whitespace();
				if self.match_current_to_punctuation("\\").is_err() {
					// don't consume any tokens
					self.current_index = index_after_expression;
				}
				// else current index has been stepped
			}

			let end = self.current_index;
			let roll_source =
				self.lexemes[start..end]
					.iter()
					.fold(String::new(), |mut accum, lexeme| {
						accum.push_str(lexeme.source());
						accum
					});
			Ok(Roll::ExplicitRoll(expression, roll_source))
		} else {
			let start = self.current_index;

			let expression = self.parse_inline_roll()?;

			let end = self.current_index;
			let roll_source =
				self.lexemes[start..end]
					.iter()
					.fold(String::new(), |mut accum, lexeme| {
						accum.push_str(lexeme.source());
						accum
					});
			Ok(Roll::InlineRoll(expression, roll_source))
		}
	}
	fn parse_inline_roll(&mut self) -> Result<Expression, ParseError> {
		let start = self.current_index;

		self.match_current_to_punctuation("[")
			.and_then(|_| self.match_current_to_punctuation("["))
			.map_err(|e| {
				self.current_index = start;
				e
			})?;

		let expression = self.parse_expression().map_err(|e| {
			self.current_index = start;
			e
		})?;

		self.skip_whitespace();
		self.match_current_to_punctuation("]")
			.and_then(|_| self.match_current_to_punctuation("]"))
			.map_err(|e| {
				self.current_index = start;
				e
			})?;

		Ok(expression)
	}

	fn parse_expression(&mut self) -> Result<Expression, ParseError> {
		let mut start_index = self.current_index;
		self.skip_whitespace();
		let md = match self.parse_mul_div() {
			Ok(md) => md,
			Err(e) => {
				self.current_index = start_index;
				return Err(e);
			}
		};
		let mut expression = Expression::MulDiv(md);

		loop {
			start_index = self.current_index;
			self.skip_whitespace();
			let is_add = if let Ok(Lexeme::Operator(token)) = self.current() {
				match token.source() {
					"+" => true,
					"-" => false,
					_ => {
						self.current_index = start_index;
						break;
					}
				}
			} else {
				self.current_index = start_index;
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
					_ => {
						self.current_index = start_index;
						break;
					}
				}
			} else {
				self.current_index = start_index;
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
			Ok(_) => match self.parse_unary() {
				Ok(unary) => return Ok(Unary::Minus(comment, Box::new(unary))),
				Err(_parse_error) => self.current_index = start_index,
			},
			Err(_parse_error) => self.current_index = start_index,
		}

		let c1 = self.parse_comment();
		let a = match self.parse_atom() {
			Ok(a) => a,
			Err(e) => {
				self.current_index = start_index;
				return Err(e);
			}
		};
		let c2 = self.parse_comment();

		let mut v = Vec::new();
		while let Ok(e) = self.parse_parentheses_expression() {
			v.push(e);
		}
		let paren_expression = if v.is_empty() { None } else { Some(v) };

		Ok(Unary::Atom(c1, Box::new(a), c2, paren_expression))
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
				let tooltip = check_tooltip(tooltip, self.parse_tooltip()).map_err(|e| {
					self.current_index = start_index;
					e
				})?;
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
			let tooltip = check_tooltip(tooltip, self.parse_tooltip()).map_err(|e| {
				self.current_index = start_index;
				e
			})?;
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

		if let Ok(expression) = self.parse_inline_roll() {
			return Ok(Atom::InlineRoll(Box::new(expression)));
		};
		self.current_index = start_index;

		if let Ok(expression) = self.parse_parentheses_expression() {
			return Ok(Atom::ParenthesesExpression(Box::new(expression)));
		}
		self.current_index = start_index;

		Err(ParseError::ExpectedOneOf(vec![
			Expecting::Dice,
			Expecting::Number,
			Expecting::RollQuery,
			Expecting::Function,
			Expecting::Macro,
			Expecting::ParenthesesExpression,
		]))
	}

	fn parse_parentheses_expression(&mut self) -> Result<Expression, ParseError> {
		let start_index = self.current_index;

		if let Err(parse_error) = self.match_current_to_punctuation("(") {
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
		if let Err(parse_error) = self.match_current_to_punctuation(")") {
			self.current_index = start_index;
			return Err(parse_error);
		}
		Ok(expression)
	}
	fn parse_function(&mut self) -> Result<Function, ParseError> {
		let start_index = self.current_index;
		let keyword = self.match_current_to_one_of_keywords(&[
			Keyword::Abs,
			Keyword::Ceil,
			Keyword::Floor,
			Keyword::Round,
			Keyword::RoundHalfDown,
		])?;

		if let Err(parse_error) = self.match_current_to_punctuation("(") {
			self.current_index = start_index;
			return Err(parse_error);
		}

		let expression = self.parse_expression().map_err(|e| {
			self.current_index = start_index;
			e
		})?;
		let expression = Box::new(expression);
		let function = match keyword {
			Keyword::Abs => Function::Abs(expression),
			Keyword::Ceil => Function::Ceil(expression),
			Keyword::Floor => Function::Floor(expression),
			Keyword::Round => Function::Round(expression),
			Keyword::RoundHalfDown => Function::RoundHalfDown(expression),
			_ => return Err(ParseError::Unknown),
		};

		self.skip_whitespace();

		if let Err(parse_error) = self.match_current_to_punctuation(")") {
			self.current_index = start_index;
			Err(parse_error)
		} else {
			Ok(function)
		}
	}

	fn parse_number(&mut self) -> Result<Number, ParseError> {
		let start_index = self.current_index;
		self.skip_whitespace();
		match self.current() {
			Ok(Lexeme::Integer(_t, i)) => {
				let i = *i;
				self.step_lexemes();
				Ok(Number::Integer(i))
			}
			Ok(Lexeme::Float(_t, f)) => {
				let f = *f;
				self.step_lexemes();
				Ok(Number::Float(f))
			}
			Ok(_) | Err(_) => {
				self.current_index = start_index;
				Err(ParseError::ExpectedOneOf(vec![
					Expecting::Integer,
					Expecting::Float,
				]))
			}
		}
	}
	fn parse_integer(&mut self) -> Result<Integer, ParseError> {
		let start_index = self.current_index;
		self.skip_whitespace();
		if let Ok(Lexeme::Integer(_, i)) = self.current() {
			let i = *i;
			self.step_lexemes();
			Ok(i)
		} else {
			self.current_index = start_index;
			Err(ParseError::Expected(Expecting::Integer))
		}
	}

	fn parse_roll_query(&mut self) -> Result<RollQuery, ParseError> {
		let start_index = self.current_index;

		self.skip_whitespace();
		self.match_current_to_punctuation("?").map_err(|e| {
			self.current_index = start_index;
			e
		})?;
		self.match_current_to_punctuation("{").map_err(|e| {
			self.current_index = start_index;
			e
		})?;

		let mut prompt = String::new();
		let mut default = String::new();
		let mut prompt_complete = false;
		loop {
			if !prompt_complete {
				match self.match_current_to_punctuation("|") {
					Ok(_) => prompt_complete = true,
					_ => {
						prompt.push_str(self.current()?.token().source());
						self.step_lexemes();
					}
				}
			} else {
				match self.match_current_to_punctuation("}") {
					Ok(_) => break,
					_ => {
						default.push_str(self.current()?.token().source());
						self.step_lexemes();
					}
				}
			}
		}

		Ok(RollQuery::new(&prompt, &default))
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
		Err(ParseError::Expected(Expecting::Dice))
	}
	fn parse_normal(&mut self) -> Result<Normal, ParseError> {
		let start_index = self.current_index;
		let count = match self.parse_integer() {
			Ok(integer) => integer,
			Err(_parse_error) => {
				self.current_index = start_index;
				Dice::default_count()
			}
		};
		self.match_current_to_keyword(Keyword::D).map_err(|e| {
			self.current_index = start_index;
			e
		})?;
		match self.parse_integer() {
			Ok(sides) => Ok(Normal { count, sides }),
			Err(parse_error) => {
				self.current_index = start_index;
				Err(parse_error)
			}
		}
	}
	fn parse_fate(&mut self) -> Result<Fate, ParseError> {
		// let start_index = self.current_index;
		// let count = self.parse_integer()?;
		// match self.match_current_to_keyword(Keyword::FateDice) {
		// 	Ok(_token) => Ok(Fate { count }),
		// 	Err(parse_error) => {
		// 		self.current_index = start_index;
		// 		Err(parse_error)
		// 	}
		// }
		Err(ParseError::Unknown)
	}
	// <computed> ::=
	//       [(<integer> | <roll_query>)] "d" "(" <expression> ")"
	//     | "(" <expression> ")" "d" (<integer> | <roll_query>)
	//     | ["(" <expression> ")"] "d" "(" <expression> ")"
	fn parse_computed(&mut self) -> Result<Computed, ParseError> {
		let count = self.parse_computed_helper()?;
		self.match_current_to_keyword(Keyword::D)?;
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
					self.match_current_to_punctuation("(")?;
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
			match self.parse_reroll_once() {
				Ok(reroll_modifier) => {
					modifiers.reroll_once = Some(reroll_modifier);
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
		if let Lexeme::Comparison(_token, cmp) = self.current()? {
			let cmp = *cmp;
			self.step_lexemes();
			Ok(cmp)
		} else {
			Err(ParseError::Expected(Expecting::Comparison))
		}
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
					Err(ParseError::Expected(Expecting::Integer))
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
				Err(ParseError::Expected(Expecting::Integer))
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
		self.match_current_to_one_of_keywords(&[Keyword::R, Keyword::Reroll])?;
		match self.parse_comparison_and_require_integer() {
			Ok((comparison, integer)) => Ok(Reroll::new(comparison, Some(integer))),
			Err(parse_error) => {
				self.current_index = start_index;
				Err(parse_error)
			}
		}
	}
	fn parse_reroll_once(&mut self) -> Result<Reroll, ParseError> {
		let start_index = self.current_index;
		self.match_current_to_keyword(Keyword::RerollOnce)?;
		match self.parse_comparison_and_require_integer() {
			Ok((comparison, integer)) => Ok(Reroll::new(comparison, Some(integer))),
			Err(parse_error) => {
				self.current_index = start_index;
				Err(parse_error)
			}
		}
	}

	fn parse_drop_keep(&mut self) -> Result<DropKeep, ParseError> {
		let start_index = self.current_index;
		let keyword = self.match_current_to_one_of_keywords(&[
			Keyword::DropHighest,
			Keyword::KeepHighest,
			Keyword::D,
			Keyword::DropLowest,
			Keyword::KeepLowest,
		])?;

		let count = match self.parse_integer() {
			Ok(int) => int,
			Err(parse_error) => {
				self.current_index = start_index;
				return Err(parse_error);
			}
		};

		match keyword {
			Keyword::DropHighest => Ok(DropKeep::DropHighest(count)),
			Keyword::KeepHighest => Ok(DropKeep::KeepHighest(count)),
			Keyword::D | Keyword::DropLowest => Ok(DropKeep::DropLowest(count)),
			Keyword::KeepLowest => Ok(DropKeep::KeepLowest(count)),
			_ => Err(ParseError::Unknown),
		}
	}
	fn parse_successes(&mut self) -> Result<Successes, ParseError> {
		let comparison = self.parse_comparison()?;
		let integer = self.parse_integer()?;
		Ok(Successes::Success(comparison, integer))
	}
	fn parse_cirtical(&mut self) -> Result<Successes, ParseError> {
		let start_index = self.current_index;

		let keyword = self.match_current_to_one_of_keywords(&[
			Keyword::CriticalSuccess,
			Keyword::CriticalFailure,
		])?;

		let (comparison, integer) = match self.parse_comparison_and_require_integer() {
			Ok((comparison, integer)) => (comparison, integer),
			Err(parse_error) => {
				self.current_index = start_index;
				return Err(parse_error);
			}
		};
		match keyword {
			Keyword::CriticalSuccess => Ok(Successes::CriticalSuccess(comparison, integer)),
			Keyword::CriticalFailure => Ok(Successes::CriticalFailure(comparison, integer)),
			_ => Err(ParseError::Unknown),
		}
	}
	fn parse_sort(&mut self) -> Result<Sort, ParseError> {
		let keyword = self
			.match_current_to_one_of_keywords(&[Keyword::SortAscending, Keyword::SortDescending])?;
		match keyword {
			Keyword::SortAscending => Ok(Sort::Ascending),
			Keyword::SortDescending => Ok(Sort::Descending),
			_ => Err(ParseError::Unknown),
		}
	}

	fn step_lexemes(&mut self) {
		self.current_index += 1;
	}
	fn step_lexemes_skip_whitespace(&mut self) {
		self.step_lexemes();
		if let Ok(Lexeme::Whitespace(_)) = self.current() {
			self.step_lexemes_skip_whitespace();
		}
	}
	fn skip_whitespace(&mut self) {
		if let Ok(Lexeme::Whitespace(_token)) = self.current() {
			self.step_lexemes();
			self.skip_whitespace();
		}
	}

	fn current(&mut self) -> Result<&Lexeme, ParseError> {
		self.current_as_option().ok_or(ParseError::OutOfBounds)
	}
	fn current_as_option(&mut self) -> Option<&Lexeme> {
		if self.current_index < self.lexemes.len() {
			return self.lexemes.get(self.current_index);
		}
		let lexeme = self.lexer.next()?;
		self.lexemes.push(lexeme);
		self.lexemes.last()
	}

	fn match_current_to_punctuation(&mut self, punctuation: &str) -> Result<(), ParseError> {
		if let Lexeme::Punctuation(token) = self.current()? {
			if token.source() == punctuation {
				self.step_lexemes();
				return Ok(());
			}
		}
		Err(ParseError::Expected(Expecting::Punctuation(
			punctuation.to_owned(),
		)))
	}
	fn match_current_to_keyword(&mut self, looking_for: Keyword) -> Result<(), ParseError> {
		if let Lexeme::Keyword(_, keyword) = self.current()? {
			if looking_for == *keyword {
				self.step_lexemes();
				return Ok(());
			}
		}
		Err(ParseError::Expected(Expecting::Keyword(
			looking_for.to_owned(),
		)))
	}
	fn match_current_to_one_of_keywords(
		&mut self,
		one_of: &[Keyword],
	) -> Result<Keyword, ParseError> {
		let err = || ParseError::Expected(Expecting::OneOfKeywords(one_of.to_vec()));

		let keyword = if let Lexeme::Keyword(_, keyword) = self.current()? {
			*keyword
		} else {
			return Err(err());
		};

		if one_of.iter().any(|k| *k == keyword) {
			self.step_lexemes();
			Ok(keyword)
		} else {
			Err(err())
		}
	}
	fn match_current_to_operator(&mut self, operator: &str) -> Result<(), ParseError> {
		if let Lexeme::Operator(token) = self.current()? {
			if token.source() == operator {
				self.step_lexemes();
				return Ok(());
			}
		}
		Err(ParseError::Expected(Expecting::Operator(
			operator.to_owned(),
		)))
	}

	fn is_roll(&mut self) -> bool {
		if self.match_current_to_operator("/").is_ok() {
			if self
				.match_current_to_one_of_keywords(&[Keyword::Roll, Keyword::R])
				.is_ok()
			{
				self.current_index -= 2;
				true
			} else {
				self.current_index -= 1;
				false
			}
		} else {
			false
		}
	}
	fn is_inline_roll(&mut self) -> bool {
		if self.match_current_to_punctuation("[").is_ok() {
			if self.match_current_to_punctuation("[").is_ok() {
				self.current_index -= 2;
				true
			} else {
				self.current_index -= 1;
				false
			}
		} else {
			false
		}
	}
}
