// File: builder.rs

use crate::interpreter::*;
use crate::ast::Expression;
use crate::macros::Macros;

use std::collections::HashMap;

pub fn default_query_prompter(message: &str, default: &str) -> Option<String> {
	use std::io;
	println!("{} default({})", message, default);
	let mut input = String::new();
	match io::stdin().read_line(&mut input) {
		Ok(_) => {
			let input = input.trim().to_owned();
			if input.is_empty() {
				Some(default.to_owned())
			} else {
				Some(input)
			}
		}
		Err(_) => None,
	}
}

pub struct InterpreterBuilder<'s, 'r, 'm> {
	source: Option<&'s str>,
	roll_queries: Option<&'r HashMap<String, Expression>>,
	macros: Option<&'m Macros>,
	query_prompter: Option<fn(&str, &str) -> Option<String>>,
}
impl<'s, 'r, 'm> Default for InterpreterBuilder<'s, 'r, 'm> {
	fn default() -> Self {
		Self {
			source: None,
			roll_queries: None,
			macros: None,
			query_prompter: None,
		}
	}
}
impl<'s, 'r, 'm> InterpreterBuilder<'s, 'r, 'm>
{
	pub fn with_source<'a>(
		&'a mut self,
		source: &'s str,
	) -> &'a mut InterpreterBuilder<'s, 'r, 'm> {
		self.source = Some(source);
		self
	}
	pub fn with_roll_queries<'a>(
		&'a mut self,
		roll_queries: &'r HashMap<String, Expression>,
	) -> &'a mut InterpreterBuilder<'s, 'r, 'm> {
		self.roll_queries = Some(roll_queries);
		self
	}
	pub fn with_macros<'a>(
		&'a mut self,
		macros: &'m Macros,
	) -> &'a mut InterpreterBuilder<'s, 'r, 'm> {
		self.macros = Some(macros);
		self
	}
	pub fn with_query_prompter<'a>(
		&'a mut self,
		prompter: fn(&str, &str) -> Option<String>,
	) -> &'a mut InterpreterBuilder<'s, 'r, 'm> {
		self.query_prompter = Some(prompter);
		self
	}

    pub fn build<R>(&self, rand: R) -> Interpreter<'s, 'm, R>
        where R: Fn() -> f64 + Copy
    {
		Interpreter::new(
			self.source.unwrap_or(""),
			self.roll_queries.unwrap_or(&HashMap::new()).clone(),
			self.macros,
			rand,
			self.query_prompter.unwrap_or(default_query_prompter),
		)
	}
}