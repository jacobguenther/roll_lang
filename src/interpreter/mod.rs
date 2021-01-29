// File: interpreter.rs

pub mod error;
pub mod output;
pub mod output_traits;
mod private_traits;

use error::InterpretError;
use output::*;
use private_traits::*;

use super::ast::*;
use super::macros::Macros;
use super::parser::{Parser, ParserT};

use std::collections::HashMap;
use std::sync::Arc;

pub struct Interpreter<'s, 'm> {
	source: &'s str,
	roll_queries: HashMap<String, Expression>,
	macros: Option<&'m Macros>,
	rand: Arc<dyn Fn() -> f64>,
	query_prmopter: fn(&str, &str) -> Option<String>,
}
impl<'s, 'm> Interpreter<'s, 'm> {
	pub fn new<'a, 'b>(
		source: &'a str,
		roll_queries: HashMap<String, Expression>,
		macros: Option<&'b Macros>,
		rand: Arc<dyn Fn() -> f64>,
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
}

pub trait InterpreterT {
	fn interpret(&mut self) -> Output;
}
impl<'s, 'm> InterpreterT for Interpreter<'s, 'm> {
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
