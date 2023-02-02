// File: interpreter.rs

pub mod error;
pub mod output;
mod private_traits;

use error::InterpretError;
use output::*;
use private_traits::*;

use super::ast::*;
use super::macros::Macros;
use super::parser::{
	Parser,
	ParserT,
};

use std::collections::HashMap;

pub struct Interpreter<'m, R>
where
	R: Fn() -> f64 + Copy,
{
	roll_queries: HashMap<String, Expression>,
	macros: Option<&'m Macros>,
	rand: R,
	query_prmopter: fn(&str, &str) -> Option<String>,
}
impl<'m, R> Interpreter<'m, R>
where
	R: Fn() -> f64 + Copy,
{
	pub fn new(
		roll_queries: HashMap<String, Expression>,
		macros: Option<&Macros>,
		rand: R,
		query_prmopter: fn(&str, &str) -> Option<String>,
	) -> Interpreter<'_, R> {
		Interpreter {
			roll_queries,
			macros,
			rand,
			query_prmopter,
		}
	}
}

pub trait InterpreterT {
	fn interpret(&mut self, source: &str) -> Output;
}
impl<'m, R> InterpreterT for Interpreter<'m, R>
where
	R: Fn() -> f64 + Copy,
{
	fn interpret(&mut self, source: &str) -> Output {
		let ast = Parser::new(source).parse();
		let mut output = Output::new(source);
		for node in ast.iter() {
			let result = match node {
				Node::ParseError(parse_error) => Err(parse_error.clone().into()),
				Node::StringLiteral(string_literal) => {
					Ok(vec![self.interpret_string_literal(string_literal)])
				}
				Node::Roll(roll) => self.interpret_roll(roll).map(|f| vec![f]),
				Node::Macro(my_macro) => self.interpret_macro(my_macro),
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
