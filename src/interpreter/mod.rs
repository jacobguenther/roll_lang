// File: interpreter.rs

#[cfg(feature = "serialize")]
use serde::{Deserialize, Serialize};

pub mod output;
pub mod output_traits;

use output::*;

use super::ast::{number::*, *};
use super::macros::Macros;
use super::parser::{ParseError, Parser, ParserT};

use std::collections::HashMap;

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
pub enum InterpretError {
	LexError,
	ParseError(ParseError),
	OperatorError(OperatorError),

	DiceWithFewerThanOneSides,
	DiceCountMustBeAnInteger,
	DiceSidesMustBeAnInteger,

	InfiniteRerollsDetected,

	FailedGettingInputFromPrompt(String),

	InterpreterConstructedWithoutMacros,
	NoMacroNamed(String),
	ErrorInMacro(String, Box<InterpretError>),
	ThisMacroCannotBeNested(String),

	Unkown,
}

pub struct Interpreter<'s, 'm> {
	source: &'s str,
	roll_queries: HashMap<String, Expression>,
	macros: Option<&'m Macros>,
	rand: fn() -> f64,
	query_prmopter: fn(&str, &str) -> Option<String>,
}
pub trait InterpreterT {
	fn new<'a, 'b>(
		source: &'a str,
		roll_queries: HashMap<String, Expression>,
		macros: Option<&'b Macros>,
		rand: fn() -> f64,
		query_prmopter: fn(&str, &str) -> Option<String>,
	) -> Interpreter<'a, 'b>;
	fn interpret(&mut self) -> Output;
}
pub trait InterpreterPrivateT {
	fn interpret_parse_error(&self, parse_error: &ParseError) -> InterpretError;
	fn interpret_string_literal(&self, string_literal: &str) -> OutputFragment;
	fn interpret_roll(&mut self, roll: &Roll) -> Result<OutputFragment, InterpretError>;
	fn interpret_macro(&mut self, my_macro: &Macro) -> Result<Vec<OutputFragment>, InterpretError>;

	fn interpret_expression(
		&mut self,
		string_literal: &Expression,
		formula: &mut FormulaFragments,
	) -> Result<Number, InterpretError>;
	fn interpret_add(
		&mut self,
		lhs: &Expression,
		rhs: &MulDiv,
		formula: &mut FormulaFragments,
	) -> Result<Number, InterpretError>;
	fn interpret_subtract(
		&mut self,
		lhs: &Expression,
		rhs: &MulDiv,
		formula: &mut FormulaFragments,
	) -> Result<Number, InterpretError>;

	fn interpret_mul_div(
		&mut self,
		mul_div: &MulDiv,
		formula: &mut FormulaFragments,
	) -> Result<Number, InterpretError>;
	fn interpret_multiply(
		&mut self,
		lhs: &MulDiv,
		rhs: &Power,
		formula: &mut FormulaFragments,
	) -> Result<Number, InterpretError>;
	fn interpret_divide(
		&mut self,
		lhs: &MulDiv,
		rhs: &Power,
		formula: &mut FormulaFragments,
	) -> Result<Number, InterpretError>;

	fn interpret_power(
		&mut self,
		power: &Power,
		formula: &mut FormulaFragments,
	) -> Result<Number, InterpretError>;

	fn interpret_unary(
		&mut self,
		unary: &Unary,
		formula: &mut FormulaFragments,
	) -> Result<Number, InterpretError>;

	fn interpret_atom(
		&mut self,
		atom: &Atom,
		formula: &mut FormulaFragments,
	) -> Result<Number, InterpretError>;
	fn interpret_dice(
		&mut self,
		dice: &Dice,
		formula: &mut FormulaFragments,
	) -> Result<Number, InterpretError>;
	fn interpret_function(
		&mut self,
		function: &Function,
		formula: &mut FormulaFragments,
	) -> Result<Number, InterpretError>;
	fn interpret_roll_query(
		&mut self,
		roll_query: &RollQuery,
		formula: &mut FormulaFragments,
	) -> Result<Number, InterpretError>;

	fn interpret_normal_dice(
		&mut self,
		normal: &Normal,
		modifiers: &Modifiers,
		tooltip: &Option<String>,
		formula: &mut FormulaFragments,
	) -> Result<Number, InterpretError>;
	fn interpret_computed_dice(
		&mut self,
		normal: &Computed,
		modifiers: &Modifiers,
		tooltip: &Option<String>,
		formula: &mut FormulaFragments,
	) -> Result<Number, InterpretError>;

	fn random_range(&self, low: i32, high: i32) -> Integer;
	fn validate_modifiers(&self, modifiers: &Modifiers, sides: i32) -> Result<(), InterpretError>;
	fn apply_exploding_and_reroll_modifiers(
		&self,
		roll: &Integer,
		sides: i32,
		modifiers: &Modifiers,
	) -> Vec<NumberRoll>;
	fn apply_drop_keep_modifiers(
		&self,
		rolls: &[NumberRoll],
		modifiers: &Modifiers,
	) -> Vec<NumberRoll>;

	fn interpret_comment(&self, option_comment: &Option<String>, formula: &mut FormulaFragments);
}
impl<'s, 'm> InterpreterT for Interpreter<'s, 'm> {
	fn new<'a, 'b>(
		source: &'a str,
		roll_queries: HashMap<String, Expression>,
		macros: Option<&'b Macros>,
		rand: fn() -> f64,
		query_prmopter: fn(&str, &str) -> Option<String>,
	) -> Interpreter<'a, 'b> {
		Interpreter {
			source,
			roll_queries,
			macros,
			rand,
			query_prmopter,
		}
	}
	fn interpret(&mut self) -> Output {
		let mut output = Output::new(&self.source);
		let ast = Parser::new(self.source).parse();
		for node in &ast {
			let result = match node {
				Node::ParseError(parse_error) => Err(self.interpret_parse_error(&parse_error)),
				Node::StringLiteral(string_literal) => {
					Ok(vec![self.interpret_string_literal(&string_literal)])
				}
				Node::Roll(roll) => match self.interpret_roll(&roll) {
					Ok(fragment) => Ok(vec![fragment]),
					Err(error) => Err(error),
				},
				Node::Macro(my_macro) => self.interpret_macro(&my_macro),
			};
			match result {
				Ok(mut fragments) => output.fragments.append(&mut fragments),
				Err(interpret_error) => {
					output.error = Some(interpret_error);
					break;
				}
			}
		}
		output
	}
}

impl<'s, 'm> InterpreterPrivateT for Interpreter<'s, 'm> {
	fn interpret_parse_error(&self, parse_error: &ParseError) -> InterpretError {
		InterpretError::ParseError(parse_error.clone())
	}
	fn interpret_string_literal(&self, string_literal: &str) -> OutputFragment {
		OutputFragment::StringLit(string_literal.to_owned())
	}

	// <roll> ::=
	//      <explicit_roll>
	//    | <inline_roll>
	// <explicit_roll> ::=
	//    "/roll" <expression> ["\"]
	// <inline_roll> ::=
	//    "[[" <expression> "]]"
	fn interpret_roll(&mut self, roll: &Roll) -> Result<OutputFragment, InterpretError> {
		let mut formula = FormulaFragments::new();
		Ok(OutputFragment::Roll(match roll {
			Roll::ExplicitRoll(expression) => {
				let result = self.interpret_expression(expression, &mut formula)?;
				RollType::ExplicitRoll(ExpressionOutput {
					formula_fragments: formula,
					result,
				})
			}
			Roll::InlineRoll(expression) => {
				let result = self.interpret_expression(expression, &mut formula)?;
				RollType::InlineRoll(ExpressionOutput {
					formula_fragments: formula,
					result,
				})
			}
		}))
	}
	fn interpret_macro(&mut self, my_macro: &Macro) -> Result<Vec<OutputFragment>, InterpretError> {
		let macro_source = match self.macros {
			Some(container) => container.get(&my_macro.name),
			None => return Err(InterpretError::InterpreterConstructedWithoutMacros),
		};

		let (output, interpreter) = match macro_source {
			Some(data) => {
				let mut interpreter = Interpreter::new(
					&data,
					self.roll_queries.clone(),
					self.macros,
					self.rand,
					self.query_prmopter,
				);
				let output = interpreter.interpret();
				(output, interpreter)
			}
			None => return Err(InterpretError::NoMacroNamed(my_macro.name.to_owned())),
		};

		if output.error.is_some() {
			Err(InterpretError::ErrorInMacro(
				my_macro.name.to_owned(),
				Box::new(output.error.unwrap()),
			))
		} else {
			self.roll_queries = interpreter.roll_queries;
			Ok(output.fragments)
		}
	}

	// <expression> ::=
	//    <expression> {("+" | "-") <mul_div>}
	fn interpret_expression(
		&mut self,
		expression: &Expression,
		formula: &mut FormulaFragments,
	) -> Result<Number, InterpretError> {
		match expression {
			Expression::Add(lhs, rhs) => self.interpret_add(lhs, rhs, formula),
			Expression::Subtract(lhs, rhs) => self.interpret_subtract(lhs, rhs, formula),
			Expression::MulDiv(mul_div) => self.interpret_mul_div(mul_div, formula),
		}
	}
	fn interpret_add(
		&mut self,
		lhs: &Expression,
		rhs: &MulDiv,
		formula: &mut FormulaFragments,
	) -> Result<Number, InterpretError> {
		let lhs = self.interpret_expression(lhs, formula)?;
		formula.push_str("+");
		let rhs = self.interpret_mul_div(rhs, formula)?;
		Ok(lhs + rhs)
	}
	fn interpret_subtract(
		&mut self,
		lhs: &Expression,
		rhs: &MulDiv,
		formula: &mut FormulaFragments,
	) -> Result<Number, InterpretError> {
		let lhs = self.interpret_expression(lhs, formula)?;
		formula.push_str("-");
		let rhs = self.interpret_mul_div(rhs, formula)?;
		Ok(lhs - rhs)
	}

	// <mul_div> ::=
	//    <mul_div> {("*" | "/") <power>}
	fn interpret_mul_div(
		&mut self,
		mul_div: &MulDiv,
		formula: &mut FormulaFragments,
	) -> Result<Number, InterpretError> {
		match mul_div {
			MulDiv::Multiply(lhs, rhs) => self.interpret_multiply(lhs, rhs, formula),
			MulDiv::Divide(lhs, rhs) => self.interpret_divide(lhs, rhs, formula),
			MulDiv::Power(power) => self.interpret_power(power, formula),
		}
	}

	fn interpret_multiply(
		&mut self,
		lhs: &MulDiv,
		rhs: &Power,
		formula: &mut FormulaFragments,
	) -> Result<Number, InterpretError> {
		let lhs = self.interpret_mul_div(lhs, formula)?;
		formula.push_str("*");
		let rhs = self.interpret_power(rhs, formula)?;
		Ok(lhs * rhs)
	}
	fn interpret_divide(
		&mut self,
		lhs: &MulDiv,
		rhs: &Power,
		formula: &mut FormulaFragments,
	) -> Result<Number, InterpretError> {
		let lhs = self.interpret_mul_div(lhs, formula)?;
		formula.push_str("/");
		let rhs = self.interpret_power(rhs, formula)?;
		match lhs / rhs {
			Ok(number) => Ok(number),
			Err(operator_error) => Err(InterpretError::OperatorError(operator_error)),
		}
	}

	// <power> ::=
	//      <unary> ("**" | "^") <power>
	//    | <unary>
	fn interpret_power(
		&mut self,
		power: &Power,
		formula: &mut FormulaFragments,
	) -> Result<Number, InterpretError> {
		match power {
			Power::Pow(u, p) => {
				let base = self.interpret_unary(u, formula)?;
				formula.push_str("^");
				let exponent = self.interpret_power(p, formula)?;
				Ok(base.pow(&exponent))
			}
			Power::Unary(unary) => self.interpret_unary(unary, formula),
		}
	}

	// <unary> ::=
	//      [<inline>] "-" <unary> =
	//    | <atom>
	fn interpret_unary(
		&mut self,
		unary: &Unary,
		formula: &mut FormulaFragments,
	) -> Result<Number, InterpretError> {
		match unary {
			Unary::Minus(comment, u) => {
				self.interpret_comment(comment, formula);
				formula.push_str("-");
				Ok(-self.interpret_unary(u, formula)?)
			}
			Unary::Atom(comment_before, atom, comment_after) => {
				self.interpret_comment(comment_before, formula);
				let result = self.interpret_atom(atom, formula)?;
				self.interpret_comment(comment_after, formula);
				Ok(result)
			}
		}
	}
	fn interpret_atom(
		&mut self,
		atom: &Atom,
		formula: &mut FormulaFragments,
	) -> Result<Number, InterpretError> {
		match atom {
			Atom::Number(number) => {
				formula.push_str(&String::from(*number));
				Ok(*number)
			}
			Atom::Dice(dice) => self.interpret_dice(dice, formula),
			Atom::Function(function) => self.interpret_function(function, formula),
			Atom::RollQuery(roll_query) => self.interpret_roll_query(roll_query, formula),
			Atom::ParenthesesExpression(expression) => {
				formula.push_str("(");
				let expression_output = self.interpret_expression(expression, formula)?;
				formula.push_str(")");
				Ok(expression_output)
			}
			Atom::InlineRoll(expression) => {
				let mut temp_formula = vec!();
				let expression_output = self.interpret_expression(expression, &mut temp_formula)?;
				formula.push_str(&format!("({})", expression_output));
				Ok(expression_output)
			}
			Atom::Macro(nested_macro) => {
				let output_fragments = self.interpret_macro(&nested_macro)?;
				if output_fragments.len() == 1 {
				if let fragment = &output_fragments[0] {
					match fragment {
						OutputFragment::Roll(RollType::InlineRoll(expression)) |
						OutputFragment::Roll(RollType::ExplicitRoll(expression)) => {
							formula.push_str(&format!("{{{}}}", expression.result));
							return Ok(expression.result);
						}
						_ => ()
					}
				}
				}
				Err(InterpretError::ThisMacroCannotBeNested(nested_macro.name.clone()))
			}
		}
	}
	fn interpret_dice(
		&mut self,
		dice: &Dice,
		formula: &mut FormulaFragments,
	) -> Result<Number, InterpretError> {
		match dice {
			Dice::Normal(normal, modifiers, tooltip) => {
				self.interpret_normal_dice(normal, modifiers, &tooltip, formula)
			}
			// Dice::Fate(fate, modifiers, tooltip) => self.interpret_fate_dice(fate, modifiers, &tooltip, formula),
			Dice::Computed(computed, modifiers, tooltip) => {
				self.interpret_computed_dice(computed, modifiers, &tooltip, formula)
			}
			_ => Err(InterpretError::Unkown),
		}
	}
	fn interpret_function(
		&mut self,
		function: &Function,
		formula: &mut FormulaFragments,
	) -> Result<Number, InterpretError> {
		let mut interpret_function_helper = |function_name: &str,
		                                     expression: &Expression,
		                                     formula: &mut FormulaFragments|
		 -> Result<Number, InterpretError> {
			formula.push_str(function_name);
			formula.push_str("(");
			let expression_output = self.interpret_expression(expression, formula)?;
			formula.push_str(")");
			Ok(expression_output)
		};
		Ok(match function {
			Function::Floor(expression) => {
				let expression_output = interpret_function_helper("floor", expression, formula)?;
				expression_output.floor()
			}
			Function::Ceil(expression) => {
				let expression_output = interpret_function_helper("ceil", expression, formula)?;
				expression_output.ceil()
			}
			Function::Round(expression) => {
				let expression_output = interpret_function_helper("round", expression, formula)?;
				expression_output.round()
			}
			Function::Abs(expression) => {
				let expression_output = interpret_function_helper("abs", expression, formula)?;
				expression_output.abs()
			}
		})
	}
	fn interpret_roll_query(
		&mut self,
		roll_query: &RollQuery,
		formula: &mut FormulaFragments,
	) -> Result<Number, InterpretError> {
		let expression = if self.roll_queries.contains_key(&roll_query.prompt) {
			self.roll_queries.get(&roll_query.prompt).unwrap().clone()
		} else {
			let user_input = match (self.query_prmopter)(&roll_query.prompt, &roll_query.default) {
				Some(input) => input,
				None => {
					return Err(InterpretError::FailedGettingInputFromPrompt(
						roll_query.prompt.clone(),
					))
				}
			};

			let expression = match Parser::parse_expression_string(&user_input) {
				Ok(expression) => expression,
				Err(parse_error) => return Err(InterpretError::ParseError(parse_error)),
			};

			self.roll_queries
				.insert(roll_query.prompt.clone(), expression.clone());
			expression
		};
		self.interpret_expression(&expression, formula)
	}

	fn interpret_normal_dice(
		&mut self,
		normal: &Normal,
		modifiers: &Modifiers,
		tooltip: &Option<String>,
		formula: &mut FormulaFragments,
	) -> Result<Number, InterpretError> {
		let sides = normal.sides.value();
		if sides < 1 {
			return Err(InterpretError::DiceWithFewerThanOneSides);
		}

		self.validate_modifiers(modifiers, sides)?;

		let mut rolls = Vec::<NumberRoll>::new();
		let mut result = 0;
		for _dice in 0..normal.count.value() {
			let roll = self.random_range(1, sides);

			let modified_rolls = self.apply_exploding_and_reroll_modifiers(&roll, sides, modifiers);
			let modified_result = modified_rolls.sum_counted_rolls();
			result += modified_result.value();

			// if compounding combine dice into single roll
			// continue to next roll
			if let Some(Expanding::Compounding(_)) = modifiers.expanding {
				rolls.push(NumberRoll::Counted(modified_result));
				continue;
			}

			let mut penetration = 0;
			for modified_roll in &modified_rolls {
				// if penetrating reduce concecutive dice rolls by 1
				if let Some(Expanding::Penetrating(_)) = modifiers.expanding {
					if let NumberRoll::Counted(num) = modified_roll {
						rolls.push(NumberRoll::Counted(
							Integer::new(num.value() - penetration).clamp_min(1),
						));
						penetration += 1;
						continue;
					}
				}
				rolls.push(*modified_roll);
			}
		}

		// Add all rolls to formula
		for roll in rolls {
			formula.push_number_roll(&roll);
		}

		match tooltip {
			Some(comment) => formula.push_tooltip(comment),
			None => (),
		}

		Ok(Number::Integer(Integer::new(result)))
	}

	fn interpret_computed_dice(
		&mut self,
		computed: &Computed,
		modifiers: &Modifiers,
		tooltip: &Option<String>,
		formula: &mut FormulaFragments,
	) -> Result<Number, InterpretError> {
		let mut count_formula = Vec::new();
		let count = self.interpret_expression(&computed.count, &mut count_formula)?;
		let mut sides_formula = Vec::new();
		let sides = self.interpret_expression(&computed.sides, &mut sides_formula)?;
		let normal = match (count, sides) {
			(Number::Integer(c), Number::Integer(s)) => Normal { count: c, sides: s },
			(Number::Float(_c), Number::Integer(_s)) => {
				return Err(InterpretError::DiceCountMustBeAnInteger)
			}
			(Number::Integer(_c), Number::Float(_s)) => {
				return Err(InterpretError::DiceSidesMustBeAnInteger)
			}
			(Number::Float(_c), Number::Float(_s)) => {
				return Err(InterpretError::DiceCountMustBeAnInteger)
			}
		};
		self.interpret_normal_dice(&normal, modifiers, tooltip, formula)
	}

	fn random_range(&self, low: i32, high: i32) -> Integer {
		Integer::new(low + ((self.rand)() * high as f64).floor() as i32)
	}
	fn validate_modifiers(&self, modifiers: &Modifiers, sides: i32) -> Result<(), InterpretError> {
		let add_rerolls_for = |comparison: &Comparison, point: i32| -> Vec<i32> {
			match comparison {
				Comparison::LessThan => (1..point).collect(),
				Comparison::GreaterThan => ((point + 1)..(sides + 1)).collect(),
				Comparison::LessThanEqual => (1..(point + 1)).collect(),
				Comparison::GreaterThanEqual => (point..(sides + 1)).collect(),
				Comparison::Equal => vec![point],
			}
		};

		let mut reroll_on = Vec::<i32>::new();
		match modifiers.expanding {
			Some(Expanding::Exploding(Exploding {
				comparison,
				comparison_point,
			}))
			| Some(Expanding::Compounding(Exploding {
				comparison,
				comparison_point,
			}))
			| Some(Expanding::Penetrating(Exploding {
				comparison,
				comparison_point,
			})) => {
				let point = comparison_point
					.unwrap_or_else(|| Integer::new(sides))
					.value();
				reroll_on.append(&mut add_rerolls_for(&comparison, point));
			}
			_ => (),
		}
		for reroll_modifier in &modifiers.reroll_modifiers {
			let point = reroll_modifier
				.comparison_point
				.unwrap_or_else(|| Integer::new(sides))
				.value();
			reroll_on.append(&mut add_rerolls_for(&reroll_modifier.comparison, point));
		}
		reroll_on.sort();
		reroll_on.dedup();
		let mut reroll_on_all = true;
		for i in 1..(sides + 1) {
			if reroll_on.iter().find(|&&x| x == i as i32).is_none() {
				reroll_on_all = false;
				break;
			}
		}

		if reroll_on_all {
			return Err(InterpretError::InfiniteRerollsDetected);
		}
		Ok(())
	}

	fn apply_exploding_and_reroll_modifiers(
		&self,
		roll: &Integer,
		sides: i32,
		modifiers: &Modifiers,
	) -> Vec<NumberRoll> {
		let mut rolls = Vec::new();

		let mut rerolled = false;
		for reroll_modifier in &modifiers.reroll_modifiers {
			let comparison_point = reroll_modifier
				.comparison_point
				.unwrap_or_else(|| Integer::new(sides));
			if roll.comparison(&reroll_modifier.comparison, &comparison_point) {
				rolls.push(NumberRoll::NotCounted(*roll));
				let new_roll = self.random_range(1, sides);
				rolls.append(
					&mut self.apply_exploding_and_reroll_modifiers(&new_roll, sides, modifiers),
				);
				rerolled = true;
				break;
			}
		}
		if !rerolled {
			rolls.push(NumberRoll::Counted(*roll));
		}

		if let Some(expanding) = modifiers.expanding {
			match expanding {
				Expanding::Compounding(exploding)
				| Expanding::Penetrating(exploding)
				| Expanding::Exploding(exploding) => {
					let comparison_point = exploding
						.comparison_point
						.unwrap_or_else(|| Integer::new(sides));
					if roll.comparison(&exploding.comparison, &comparison_point) {
						let new_roll = self.random_range(1, sides);
						rolls.append(
							&mut self
								.apply_exploding_and_reroll_modifiers(&new_roll, sides, modifiers),
						);
					}
				}
			}
		}

		rolls
	}
	fn apply_drop_keep_modifiers(
		&self,
		_rolls: &[NumberRoll],
		_modifiers: &Modifiers,
	) -> Vec<NumberRoll> {
		vec![]
	}

	fn interpret_comment(&self, option_comment: &Option<String>, formula: &mut FormulaFragments) {
		match option_comment {
			Some(comment) => formula.push_str(&format!("[{}]", comment)),
			None => (),
		}
	}
}
