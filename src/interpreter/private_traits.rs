// File: interpreter/private_traits.rs

use crate::ast::{number::*, *};
use crate::parser::{error::ParseError, *};

use super::error::{InterpretError, NotSupportedYet};
use super::output::*;
use super::{Interpreter, InterpreterT};

pub(super) trait InterpreterPrivateT {
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
		tooltip: &Option<String>,
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
	fn calc_normal_dice_from_computed_dice(
		&mut self,
		computed: &Computed,
	) -> Result<Normal, InterpretError>;

	fn random_range(&self, low: Integer, high: Integer) -> Integer;
	fn validate_modifiers(
		&self,
		modifiers: &Modifiers,
		sides: Integer,
	) -> Result<(), InterpretError>;

	fn reroll_applies(&self, roll: Integer, sides: Integer, modifiers: &[Reroll]) -> bool;

	fn apply_reroll_modifiers(
		&self,
		next_roll_value: Integer,
		rolls: &mut Vec<NumberRoll>,
		sides: Integer,
		modifiers: &[Reroll],
	);

	fn apply_exploding_modifiers(
		&self,
		rolls: &mut Vec<NumberRoll>,
		sides: Integer,
		modifiers: &[Reroll],
	);
	fn apply_penetrating_modifiers(
		&self,
		rolls: &mut Vec<NumberRoll>,
		sides: Integer,
		modifiers: &[Reroll],
		is_first: bool,
	);
	fn apply_compounding_modifiers(
		&self,
		rolls: &mut Vec<NumberRoll>,
		sides: Integer,
		modifiers: &[Reroll],
	);
	fn apply_drop_keep_modifiers(&self, rolls: &mut Vec<NumberRoll>, modifiers: &Modifiers);
	fn apply_sort_modifier(&self, rolls: &mut Vec<NumberRoll>, sort: Option<Sort>);

	fn interpret_comment(&self, option_comment: &Option<String>, formula: &mut FormulaFragments);
}

impl<'s, 'm, R> InterpreterPrivateT for Interpreter<'s, 'm, R>
where
	R: Fn() -> f64 + Copy,
{
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
					data,
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
		formula.push_str(" + ");
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
		formula.push_str(" - ");
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
		formula.push_str(" * ");
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
		formula.push_str(" / ");
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
				let result = -self.interpret_unary(u, formula)?;

				Ok(result)
			}
			Unary::Atom(leading_comment, atom, trailing_comment) => {
				self.interpret_comment(leading_comment, formula);
				let result = self.interpret_atom(atom, formula)?;
				self.interpret_comment(trailing_comment, formula);
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
			Atom::Number(number, tooltip) => {
				formula.push_new_str(&String::from(*number));
				if let Some(tip) = tooltip {
					formula.push_tooltip(tip);
				}
				Ok(*number)
			}
			Atom::Dice(dice, tooltip) => self.interpret_dice(dice, tooltip, formula),
			Atom::Function(function) => self.interpret_function(function, formula),
			Atom::RollQuery(roll_query) => self.interpret_roll_query(roll_query, formula),
			Atom::ParenthesesExpression(expression) => {
				formula.push_new_str("(");
				let expression_output = self.interpret_expression(expression, formula)?;
				formula.push_str(")");
				Ok(expression_output)
			}
			Atom::InlineRoll(expression) => {
				let mut temp_formula = vec![];
				let expression_output = self.interpret_expression(expression, &mut temp_formula)?;
				formula.push_new_str(&format!("({})", expression_output));
				Ok(expression_output)
			}
			Atom::Macro(nested_macro) => {
				let output_fragments = self.interpret_macro(nested_macro)?;
				if output_fragments.len() == 1 {
					let fragment = &output_fragments[0];
					match fragment {
						OutputFragment::Roll(RollType::InlineRoll(expression))
						| OutputFragment::Roll(RollType::ExplicitRoll(expression)) => {
							formula.push_str("{");
							formula.append(&mut expression.formula_fragments.clone());
							formula.push_str("}");
							return Ok(expression.result);
						}
						_ => (),
					}
				}
				Err(InterpretError::ThisMacroCannotBeNested(
					nested_macro.name.clone(),
				))
			}
		}
	}
	fn interpret_dice(
		&mut self,
		dice: &Dice,
		tooltip: &Option<String>,
		formula: &mut FormulaFragments,
	) -> Result<Number, InterpretError> {
		match dice {
			Dice::Normal(normal, modifiers) => {
				self.interpret_normal_dice(normal, modifiers, tooltip, formula)
			}
			Dice::Fate(_fate, _modifiers) => Err(InterpretError::NotSupportedYet(NotSupportedYet::FateDice)),
			// self.interpret_fate_dice(fate, modifiers, tooltip, formula),
			Dice::Computed(computed, modifiers) => {
				let normal = self.calc_normal_dice_from_computed_dice(computed)?;
				self.interpret_normal_dice(&normal, modifiers, tooltip, formula)
			}
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
			Function::RoundHalfDown(expression) => {
				let expression_output =
					interpret_function_helper("round_half_down", expression, formula)?;
				expression_output.round_half_down()
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
		let sides = normal.sides;
		if sides < 1 {
			return Err(InterpretError::DiceWithFewerThanOneSides);
		}

		self.validate_modifiers(modifiers, sides)?;

		let mut rolls = (0..normal.count)
			.flat_map(|_i| {
				let roll = self.random_range(1, sides);

				let mut modified_rolls = Vec::new();
				self.apply_reroll_modifiers(roll, &mut modified_rolls, sides, &modifiers.reroll);

				if let Some(expanding) = modifiers.expanding.as_ref() {
					match expanding {
						Expanding::Exploding(exploding) => {
							self.apply_exploding_modifiers(&mut modified_rolls, sides, exploding)
						}
						Expanding::Penetrating(penetrating) => self.apply_penetrating_modifiers(
							&mut modified_rolls,
							sides,
							penetrating,
							true,
						),
						Expanding::Compounding(compounding) => self.apply_compounding_modifiers(
							&mut modified_rolls,
							sides,
							compounding,
						),
					}
				}

				// self.apply_post_modifiers(&mut modified_rolls, modifiers.post_modifiers);

				modified_rolls
			})
			.collect::<Vec<_>>();

		self.apply_drop_keep_modifiers(&mut rolls, modifiers);

		self.apply_sort_modifier(&mut rolls, modifiers.sort);

		let result = rolls.iter().fold(0, |mut acc, el| {
			if let NumberRoll::Counted(roll) = el {
				acc += roll;
			}
			acc
		});

		// Add all rolls to formula
		for roll in rolls.iter() {
			formula.push_number_roll(roll);
		}
		if let Some(tip) = tooltip {
			formula.push_tooltip(tip);
		}

		Ok(Number::Integer(result))
	}

	fn calc_normal_dice_from_computed_dice(
		&mut self,
		computed: &Computed,
	) -> Result<Normal, InterpretError> {
		let mut count_formula = Vec::new();
		let count = self.interpret_expression(&computed.count, &mut count_formula)?;
		let mut sides_formula = Vec::new();
		let sides = self.interpret_expression(&computed.sides, &mut sides_formula)?;
		match (count, sides) {
			(Number::Integer(count), Number::Integer(sides)) => Ok(Normal { count, sides }),
			(Number::Float(_count), Number::Integer(_sides)) => {
				Err(InterpretError::DiceCountMustBeAnInteger)
			}
			(Number::Integer(_count), Number::Float(_sides)) => {
				Err(InterpretError::DiceSidesMustBeAnInteger)
			}
			(Number::Float(_count), Number::Float(_sides)) => {
				Err(InterpretError::DiceCountMustBeAnInteger)
			}
		}
	}

	fn random_range(&self, low: Integer, high: Integer) -> Integer {
		low + ((self.rand)() * high as Float).floor() as Integer
	}
	fn validate_modifiers(
		&self,
		modifiers: &Modifiers,
		sides: Integer,
	) -> Result<(), InterpretError> {
		let get_rerolls_for = |comparison: &Comparison, point: Integer| -> Vec<Integer> {
			match comparison {
				Comparison::LessThan => (1..point).collect(),
				Comparison::GreaterThan => ((point + 1)..(sides + 1)).collect(),
				Comparison::LessThanEqual => (1..(point + 1)).collect(),
				Comparison::GreaterThanEqual => (point..(sides + 1)).collect(),
				Comparison::Equal => vec![point],
			}
		};

		let rerolls_iter = modifiers.reroll.iter();
		let temp = Vec::new();
		let all_rerolls_iter = if let Some(expanding_modifier) = modifiers.expanding.as_ref() {
			match expanding_modifier {
				Expanding::Exploding(rerolls)
				| Expanding::Penetrating(rerolls)
				| Expanding::Compounding(rerolls) => rerolls_iter.chain(rerolls.iter()),
			}
		} else {
			rerolls_iter.chain(temp.iter())
		};

		let mut reroll_on = all_rerolls_iter
			.flat_map(
				|&Reroll {
				     comparison_point,
				     comparison,
				 }| {
					let point = comparison_point.unwrap_or(sides);
					get_rerolls_for(&comparison, point)
				},
			)
			.collect::<Vec<_>>();

		reroll_on.sort_unstable();
		reroll_on.dedup();

		let mut reroll_on_all = true;
		for i in 1..(sides + 1) {
			if !reroll_on.iter().any(|&x| x == i) {
				reroll_on_all = false;
				break;
			}
		}

		if reroll_on_all {
			return Err(InterpretError::InfiniteRerollsDetected);
		}
		Ok(())
	}

	fn reroll_applies(&self, roll: Integer, sides: Integer, modifiers: &[Reroll]) -> bool {
		for reroll_modifier in modifiers.iter() {
			let comparison_point = reroll_modifier.comparison_point.unwrap_or(sides);
			if compare_integers(&reroll_modifier.comparison, roll, comparison_point) {
				return true;
			}
		}
		false
	}

	fn apply_reroll_modifiers(
		&self,
		next_roll_value: Integer,
		rolls: &mut Vec<NumberRoll>,
		sides: Integer,
		reroll_modifiers: &[Reroll],
	) {
		if reroll_modifiers.is_empty() {
			rolls.push(NumberRoll::Counted(next_roll_value))
		} else if self.reroll_applies(next_roll_value, sides, reroll_modifiers) {
			rolls.push(NumberRoll::NotCounted(next_roll_value));
			let new_roll = self.random_range(1, sides);
			self.apply_reroll_modifiers(new_roll, rolls, sides, reroll_modifiers);
		} else {
			rolls.push(NumberRoll::Counted(next_roll_value));
		}
	}

	fn apply_exploding_modifiers(
		&self,
		rolls: &mut Vec<NumberRoll>,
		sides: Integer,
		modifiers: &[Reroll],
	) {
		if let Some(NumberRoll::Counted(counted)) = rolls.last() {
			for modifier in modifiers.iter() {
				let comparison_point = modifier.comparison_point.unwrap_or(sides);
				if compare_integers(&modifier.comparison, *counted, comparison_point) {
					let new_roll = self.random_range(1, sides);
					rolls.push(NumberRoll::Counted(new_roll));
					self.apply_exploding_modifiers(rolls, sides, modifiers);
					break;
				}
			}
		}
	}
	fn apply_penetrating_modifiers(
		&self,
		rolls: &mut Vec<NumberRoll>,
		sides: Integer,
		modifiers: &[Reroll],
		is_first: bool,
	) {
		if let Some(NumberRoll::Counted(counted)) = rolls.last() {
			let do_penetrating_roll = if is_first {
				self.reroll_applies(*counted, sides, modifiers)
			} else {
				self.reroll_applies(counted + 1, sides, modifiers)
			};
			if do_penetrating_roll {
				let raw_roll = self.random_range(1, sides);
				rolls.push(NumberRoll::Counted(raw_roll - 1));
				self.apply_penetrating_modifiers(rolls, sides, modifiers, false);
			}
		}
	}
	fn apply_compounding_modifiers(
		&self,
		rolls: &mut Vec<NumberRoll>,
		sides: Integer,
		modifiers: &[Reroll],
	) {
		if let Some(NumberRoll::Counted(counted)) = rolls.last_mut() {
			if self.reroll_applies(*counted, sides, modifiers) {
				loop {
					let next_roll = self.random_range(1, sides);
					*counted += next_roll;
					if !self.reroll_applies(next_roll, sides, modifiers) {
						break;
					}
				}
			}
		}
	}

	fn apply_drop_keep_modifiers(&self, rolls: &mut Vec<NumberRoll>, modifiers: &Modifiers) {
		if let Some(dk) = modifiers.drop_keep {
			let mut sorted = rolls.clone();
			sorted.sort_unstable();

			let drop_helper =
				|rolls: &mut Vec<NumberRoll>, count, target, cmp: fn(Integer, Integer) -> bool| {
					let mut drop_count = 0;
					for roll in rolls.iter_mut() {
						if let NumberRoll::Counted(value) = roll {
							if cmp(*value, target) {
								*roll = NumberRoll::NotCounted(*value);
								drop_count += 1;
							}
							if drop_count == count {
								break;
							}
						}
					}
				};
			let keep_helper =
				|rolls: &mut Vec<NumberRoll>, count, target, cmp: fn(Integer, Integer) -> bool| {
					let mut keep_count = 0;
					for roll in rolls.iter_mut() {
						if let NumberRoll::Counted(value) = roll {
							if cmp(*value, target) || (*value == target && keep_count >= count) {
								*roll = NumberRoll::NotCounted(*value);
							} else {
								keep_count += 1;
							}
						}
					}
				};

			match dk {
				DropKeep::DropLowest(count) => {
					let target = sorted[count as usize - 1].value();
					drop_helper(rolls, count, target, |a, b| a <= b);
				}
				DropKeep::DropHighest(count) => {
					let target = sorted[sorted.len() - count as usize].value();
					drop_helper(rolls, count, target, |a, b| a >= b);
				}
				DropKeep::KeepLowest(count) => {
					let target = sorted[count as usize - 1].value();
					keep_helper(rolls, count, target, |a, b| a > b);
				}
				DropKeep::KeepHighest(count) => {
					let target = sorted[sorted.len() - count as usize].value();
					keep_helper(rolls, count, target, |a, b| a < b);
				}
			}
		}
	}

	fn apply_sort_modifier(&self, rolls: &mut Vec<NumberRoll>, sort: Option<Sort>) {
		let to_values = |a: &NumberRoll, b: &NumberRoll| -> (Integer, Integer) {
			let a = match a {
				NumberRoll::Counted(v) | NumberRoll::NotCounted(v) => v
			};
			let b = match b {
				NumberRoll::Counted(v) | NumberRoll::NotCounted(v) => v
			};
			(*a, *b)
		};
		if let Some(sort) = sort {
			match sort {
				Sort::Ascending => rolls.sort_by(|a: &NumberRoll, b: &NumberRoll| {
					let (a, b) = to_values(a, b);
					a.cmp(&b)
				}),
				Sort::Decending => rolls.sort_by(|a: &NumberRoll, b: &NumberRoll| {
					let (a, b) = to_values(a, b);
					b.cmp(&a)
				}),
			}
		}
	}
	fn interpret_comment(&self, option_comment: &Option<String>, formula: &mut FormulaFragments) {
		match option_comment {
			Some(comment) => formula.push_str(&format!("[{}]", comment)),
			None => (),
		}
	}
}
