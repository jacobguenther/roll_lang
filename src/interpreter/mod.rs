// File: interpreter.rs

pub mod output;
use output::*;

use super::ast::{
	*,
	number::*,
};
use super::parser::{
	Parser,
	ParserT,
	ParseError,
};

use std::convert::TryInto;


#[derive(Debug, Clone)]
pub enum InterpretError {
	LexError,
	ParseError(ParseError),
	OperatorError(OperatorError),

	RandomNumberGenerator,

	Unkown,
}

pub struct Interpreter {
	source: String,
	ast: Root,
}
pub trait InterpreterT {
	fn new(source: &str) -> Interpreter;
	fn interpret(&self) -> Output;
}
pub trait InterpreterPrivateT {
	fn interpret_parse_error(&self, parse_error: &ParseError) -> InterpretError;
	fn interpret_string_literal(&self, string_literal: &StringLiteral) -> OutputFragment;
	fn interpret_roll(&self, roll: &Roll) -> Result<OutputFragment, InterpretError>;

	fn interpret_expression(&self, string_literal: &Expression, formula: &mut FormulaFragments) -> Result<Number, InterpretError>;
	fn interpret_add(&self, lhs: &Expression, rhs: &MulDiv, formula: &mut FormulaFragments) -> Result<Number, InterpretError>;
	fn interpret_subtract(&self, lhs: &Expression, rhs: &MulDiv, formula: &mut FormulaFragments) -> Result<Number, InterpretError>;

	fn interpret_mul_div(&self, mul_div: &MulDiv, formula: &mut FormulaFragments) -> Result<Number, InterpretError>;
	fn interpret_multiply(&self, lhs: &MulDiv, rhs: &Power, formula: &mut FormulaFragments) -> Result<Number, InterpretError>;
	fn interpret_divide(&self, lhs: &MulDiv, rhs: &Power, formula: &mut FormulaFragments) -> Result<Number, InterpretError>;

	fn interpret_power(&self, power: &Power, formula: &mut FormulaFragments) -> Result<Number, InterpretError>;

	fn interpret_unary(&self, unary: &Unary, formula: &mut FormulaFragments) -> Result<Number, InterpretError>;

	fn interpret_atom(&self, atom: &Atom, formula: &mut FormulaFragments) -> Result<Number, InterpretError>;
	fn interpret_dice(&self, dice: &Dice, formula: &mut FormulaFragments) -> Result<Number, InterpretError>;
	fn interpret_normal_dice(&self, normal: &Normal, modifiers: &Vec<Modifier>, tooltip: &Option<InlineComment>, formula: &mut FormulaFragments) -> Result<Number, InterpretError>;
	fn interpret_function(&self, function: &Function, formula: &mut FormulaFragments) -> Result<Number, InterpretError>;
	fn interpret_roll_query(&self, roll_query: &RollQuery, formula: &mut FormulaFragments) -> Result<Number, InterpretError>;

	fn interpret_comment(&self, option_comment: &Option<InlineComment>, formula: &mut FormulaFragments);
}
impl InterpreterT for Interpreter {
	fn new(source: &str) -> Interpreter {
		Interpreter {
			source: source.to_owned(),
			ast: Parser::new(source).parse(),
		}
	}
	fn interpret(&self) -> Output {
		let mut output = Output::new(&self.source);
		for node in &self.ast {
			let result = match node {
				Node::ParseError(parse_error) => Err(self.interpret_parse_error(&parse_error)),
				Node::StringLiteral(string_literal) => Ok(self.interpret_string_literal(&&string_literal)),
				Node::Roll(roll) => self.interpret_roll(&roll),
			};
			match result {
				Ok(fragment) => output.fragments.push(fragment),
				Err(interpret_error) => {
					output.error = Some(interpret_error);
					break;
				}
			}
		}
		output
	}
}
impl InterpreterPrivateT for Interpreter {
	fn interpret_parse_error(&self, parse_error: &ParseError) -> InterpretError {
		InterpretError::ParseError(parse_error.clone())
	}
	fn interpret_string_literal(&self, string_literal: &StringLiteral) -> OutputFragment {
		OutputFragment::StringLit(string_literal.str().to_owned())
	}

	// <roll> ::=
	//      <explicit_roll>
	//    | <inline_roll>
	// <explicit_roll> ::=
	//    "/roll" <expression> ["\"]
	// <inline_roll> ::=
	//    "[[" <expression> "]]"
	fn interpret_roll(&self, roll: &Roll) -> Result<OutputFragment, InterpretError> {
		let mut formula = FormulaFragments::new();
		Ok(OutputFragment::Roll(match roll {
			Roll::ExplicitRoll(expression) => {
				let result = self.interpret_expression(expression, &mut formula)?;
				RollType::ExplicitRoll(ExpressionOutput {
					formula_fragments: formula.clone(),
					result: result
				})
			},
			Roll::InlineRoll(expression) => {
				let result = self.interpret_expression(expression, &mut formula)?;
				RollType::InlineRoll(ExpressionOutput {
					formula_fragments: formula.clone(),
					result: result
				})
			}
		}))
	}


	// <expression> ::=
	//    <expression> {("+" | "-") <mul_div>}
	fn interpret_expression(&self, expression: &Expression, formula: &mut FormulaFragments) -> Result<Number, InterpretError> {
		match expression {
			Expression::Add(lhs, rhs) => self.interpret_add(lhs, rhs, formula),
			Expression::Subtract(lhs, rhs) => self.interpret_subtract(lhs, rhs, formula),
			Expression::MulDiv(mul_div) => self.interpret_mul_div(mul_div, formula),
		}
	}
	fn interpret_add(&self, lhs: &Expression, rhs: &MulDiv, formula: &mut FormulaFragments)  -> Result<Number, InterpretError> {
		let lhs = self.interpret_expression(lhs, formula)?;
		formula.push_str("+");
		let rhs = self.interpret_mul_div(rhs, formula)?;
		Ok(lhs + rhs)
	}
	fn interpret_subtract(&self, lhs: &Expression, rhs: &MulDiv, formula: &mut FormulaFragments)  -> Result<Number, InterpretError> {
		let lhs = self.interpret_expression(lhs, formula)?;
		formula.push_str("-");
		let rhs = self.interpret_mul_div(rhs, formula)?;
		Ok(lhs - rhs)
	}

	// <mul_div> ::=
	//    <mul_div> {("*" | "/") <power>}
	fn interpret_mul_div(&self, mul_div: &MulDiv, formula: &mut FormulaFragments)  -> Result<Number, InterpretError> {
		match mul_div {
			MulDiv::Multiply(lhs, rhs) => self.interpret_multiply(lhs, rhs, formula),
			MulDiv::Divide(lhs, rhs) => self.interpret_divide(lhs, rhs, formula),
			MulDiv::Power(power) => self.interpret_power(power, formula),
		}
	}

	fn interpret_multiply(&self, lhs: &MulDiv, rhs: &Power, formula: &mut FormulaFragments) -> Result<Number, InterpretError> {
		let lhs = self.interpret_mul_div(lhs, formula)?;
		formula.push_str("*");
		let rhs = self.interpret_power(rhs, formula)?;
		Ok(lhs * rhs)
	}
	fn interpret_divide(&self, lhs: &MulDiv, rhs: &Power, formula: &mut FormulaFragments) -> Result<Number, InterpretError> {
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
	fn interpret_power(&self, power: &Power, formula: &mut FormulaFragments) -> Result<Number, InterpretError> {
		match power {
			Power::Pow(u, p) => {
				let base = self.interpret_unary(u, formula)?;
				formula.push_str("^");
				let exponent = self.interpret_power(p, formula)?;
				Ok(base.pow(&exponent))
			},
			Power::Unary(unary) => self.interpret_unary(unary, formula),
		}
	}

	// <unary> ::=
	//      [<inline>] "-" <unary> =
	//    | <atom>
	fn interpret_unary(&self, unary: &Unary, formula: &mut FormulaFragments) -> Result<Number, InterpretError> {
		match unary {
			Unary::Minus(comment, u) => {
				self.interpret_comment(comment, formula);
				formula.push_str("-");
				Ok(-self.interpret_unary(u, formula)?)
			},
			Unary::Atom(atom) => self.interpret_atom(atom, formula),
		}
	}
	fn interpret_atom(&self, atom: &Atom, formula: &mut FormulaFragments) -> Result<Number, InterpretError> {
		match atom {
			Atom::Number(comment_before, number, comment_after) => {
				self.interpret_comment(comment_before, formula);
				formula.push_str(&String::from(*number));
				self.interpret_comment(comment_after, formula);
				Ok(*number)
			},
			Atom::Dice(comment_before, dice, comment_after) => {
				self.interpret_comment(comment_before, formula);
				let dice_result = self.interpret_dice(dice, formula)?;
				self.interpret_comment(comment_after, formula);
				Ok(dice_result)
			},
			Atom::Function(comment_before, function, comment_after) => {
				self.interpret_comment(comment_before, formula);
				let function_result = self.interpret_function(function, formula)?;
				self.interpret_comment(comment_after, formula);
				Ok(function_result)
			},
			Atom::RollQuery(comment_before, roll_query, comment_after) => {
				self.interpret_comment(comment_before, formula);
				let roll_query_result = self.interpret_roll_query(roll_query, formula)?;
				self.interpret_comment(comment_after, formula);
				Ok(roll_query_result)
			},
			Atom::Expression(comment_before, expression, comment_after) => {
				self.interpret_comment(comment_before, formula);
				formula.push_str("(");
				let expression_output = self.interpret_expression(expression, formula)?;
				formula.push_str(")");
				self.interpret_comment(comment_after, formula);
				Ok(expression_output)
			}
		}
	}
	fn interpret_dice(&self, dice: &Dice, formula: &mut FormulaFragments) -> Result<Number, InterpretError> {
		match dice {
			Dice::Normal(normal, modifiers, tooltip) => self.interpret_normal_dice(normal, modifiers, &tooltip, formula),
			// Dice::Fate(fate, modifiers, tooltip) => self.interpret_fate_dice(fate, modifiers, &tooltip, formula),
			// Dice::Computed(computed, modifiers, tooltip) => self.interpret_computed_dice(computed, modifiers, &tooltip, formula),
			_ => Err(InterpretError::Unkown),
		}
	}
	fn interpret_normal_dice(&self, normal: &Normal, modifiers: &Vec<Modifier>, tooltip: &Option<InlineComment>, formula: &mut FormulaFragments) -> Result<Number, InterpretError> {
		let random_range = | low: i32, high: i32 | -> Integer {
			use js_sys::Math::random;
			Integer::new((low as f64 + random() * (high-1) as f64).ceil() as i32)
		};
		let interpret_modifiers = | modifiers: &Vec<Modifier> | -> () {

		};

		formula.push_str("(");
		let mut result = 0;
		let sides = normal.sides.value();
		for dice in 0..normal.count.value() {
			let roll_value = random_range(1, sides);
			formula.push_number_roll(&roll_value);
			result += roll_value.value();
		}
		formula.push_str(")");
		Ok(Number::Integer(Integer::new(result)))
	}

	fn interpret_function(&self, function: &Function, formula: &mut FormulaFragments) -> Result<Number, InterpretError> {
		let interpret_function_helper = |function_name: &str, expression: &Expression, formula: &mut FormulaFragments| -> Result<Number, InterpretError> {
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
			},
			Function::Ceil(expression) => {
				let expression_output = interpret_function_helper("ceil", expression, formula)?;
				expression_output.ceil()
			},
			Function::Round(expression) => {
				let expression_output = interpret_function_helper("round", expression, formula)?;
				expression_output.round()
			},
			Function::Abs(expression) => {
				let expression_output = interpret_function_helper("abs", expression, formula)?;
				expression_output.abs()
			},
		})
	}
	fn interpret_roll_query(&self, _roll_query: &RollQuery, _formula: &mut FormulaFragments) -> Result<Number, InterpretError> {
		Err(InterpretError::Unkown)
	}
	fn interpret_comment(&self, option_comment: &Option<InlineComment>, formula: &mut FormulaFragments) {
		match option_comment {
			Some(comment) => formula.push_str(&format!("[{}]", comment.comment())),
			None => (),
		}
	}
}

