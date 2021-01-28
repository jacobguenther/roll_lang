// File: lib.rs

extern crate unicode_segmentation;

pub mod ast;
pub mod interpreter;
pub mod lexer;
pub mod macros;
pub mod parser;

use interpreter::*;
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
	roll_queries: Option<&'r HashMap<String, ast::Expression>>,
	macros: Option<&'m macros::Macros>,
	rand: Option<fn() -> f64>,
	query_prompter: Option<fn(&str, &str) -> Option<String>>,
}
impl<'s, 'r, 'm> Default for InterpreterBuilder<'s, 'r, 'm> {
	fn default() -> InterpreterBuilder<'s, 'r, 'm> {
		InterpreterBuilder::new()
	}
}
impl<'s, 'r, 'm> InterpreterBuilder<'s, 'r, 'm> {
	pub fn new() -> InterpreterBuilder<'s, 'r, 'm> {
		InterpreterBuilder {
			source: None,
			roll_queries: None,
			macros: None,
			rand: None,
			query_prompter: None,
		}
	}

	pub fn with_source<'a>(
		&'a mut self,
		source: &'s str,
	) -> &'a mut InterpreterBuilder<'s, 'r, 'm> {
		self.source = Some(source);
		self
	}
	pub fn with_roll_queries<'a>(
		&'a mut self,
		roll_queries: &'r HashMap<String, ast::Expression>,
	) -> &'a mut InterpreterBuilder<'s, 'r, 'm> {
		self.roll_queries = Some(roll_queries);
		self
	}
	pub fn with_macros<'a>(
		&'a mut self,
		macros: &'m macros::Macros,
	) -> &'a mut InterpreterBuilder<'s, 'r, 'm> {
		self.macros = Some(macros);
		self
	}
	pub fn with_rng_func<'a>(
		&'a mut self,
		rand: fn() -> f64,
	) -> &'a mut InterpreterBuilder<'s, 'r, 'm> {
		self.rand = Some(rand);
		self
	}
	pub fn with_query_prompter<'a>(
		&'a mut self,
		prompter: fn(&str, &str) -> Option<String>,
	) -> &'a mut InterpreterBuilder<'s, 'r, 'm> {
		self.query_prompter = Some(prompter);
		self
	}

	pub fn build(&self) -> Interpreter<'s, 'm> {
		Interpreter::new(
			self.source.unwrap_or(""),
			self.roll_queries.unwrap_or(&HashMap::new()).clone(),
			self.macros,
			self.rand.unwrap(),
			self.query_prompter.unwrap_or(default_query_prompter),
		)
	}
}

#[cfg(test)]
pub mod tests {
	use super::*;

	fn r() -> f64 {
		1.0
	}
	fn helper(source: &str, result: &str) {
		let output = InterpreterBuilder::new()
			.with_source(&source)
			.with_rng_func(r)
			.build()
			.interpret()
			.to_string();
		println!("{}", source);
		assert_eq!(&output, result);
	}

	#[test]
	fn interpreter() {
		// associativity
		helper("[[5-4+1]]", "(2)");
		// multiply and divide
		helper("[[4*6/3]]", "(8)");
		// precedence
		helper("[[(4+2)*2]]", "(12)");

		// unicode and localization
		helper("文字 hello", "文字 hello");

		// whitespaces
		helper("[[ 20 + 4 * 2 ]]", "(28)");
		helper("[[ 20 + 4 * 2 ]] ", "(28) ");
		helper("/r 20 + 4 * 2 ", "20 + 4 * 2 = 28 ");
		helper("/r 20 + 4 * 2 \\", "20 + 4 * 2 = 28");
		helper("/r 20 + 4 * 2 \\ ", "20 + 4 * 2 = 28 ");

		// comment
		helper("/r [1]20 \\", "[1]20 = 20");
		helper("/r 20[1] \\", "20[1] = 20");
		// comments
		helper("/r [1]20[2] \\", "[1]20[2] = 20");

		// tooltips
		helper("/r [?1]20 \\", "[20 | tip: 1] = 20");
		helper("/r 20[?1] \\", "[20 | tip: 1] = 20");

		// comment and tooltip
		helper("/r [1][?2]20 \\", "[1][20 | tip: 2] = 20");
		helper("/r [1]20[?2] \\", "[1][20 | tip: 2] = 20");
		helper("/r 20[?1][2] \\", "[20 | tip: 1][2] = 20");

		// comments and tooltip
		helper("/r [1][?2]20[3] \\", "[1][20 | tip: 2][3] = 20");
		helper("/r [1]20[?2][3] \\", "[1][20 | tip: 2][3] = 20");

		// comment
		helper("/r [1]1d1 \\", "[1](2) = 2");
		helper("/r 1d1[1] \\", "(2)[1] = 2");
		// comments
		helper("/r [1]1d1[2] \\", "[1](2)[2] = 2");

		// tooltips
		helper("/r [?1]1d1 \\", "[(2) | tip: 1] = 2");
		helper("/r 1d1[?1] \\", "[(2) | tip: 1] = 2");

		// comment and tooltip
		helper("/r [1][?2]1d1 \\", "[1][(2) | tip: 2] = 2");
		helper("/r [1]1d1[?2] \\", "[1][(2) | tip: 2] = 2");
		helper("/r 1d1[?1][2] \\", "[(2) | tip: 1][2] = 2");

		// comments and tooltip
		helper("/r [1][?2]1d1[3] \\", "[1][(2) | tip: 2][3] = 2");
		helper("/r [1]1d1[?2][3] \\", "[1][(2) | tip: 2][3] = 2");

		// comment and tooltip
		helper("/r [1][?2]3d1 \\", "[1][(2+2+2) | tip: 2] = 6");
		helper("/r [1]4d1[?2] \\", "[1][(2+2+2+2) | tip: 2] = 8");
		helper("/r 5d1[?1][2] \\", "[(2+2+2+2+2) | tip: 1][2] = 10");
	}

	use macros::*;
	fn macro_helper(macros: &Macros, source: &str, result: &str) {
		let interpreter_result = InterpreterBuilder::new()
			.with_source(&source)
			.with_macros(&macros)
			.with_rng_func(r)
			.build()
			.interpret()
			.to_string();
		assert_eq!(interpreter_result, result);
	}

	#[test]
	fn top_level_macro() {
		use macros::*;
		let mut macros = Macros::new();
		macros.insert(String::from("melee attack"), String::from("[[15+4]]"));
		macro_helper(&macros, "#{melee attack}", "(19)");
	}
	#[test]
	fn top_level_short_macro() {
		use macros::*;
		let mut macros = Macros::new();
		macros.insert(String::from("melee"), String::from("[[15+4]]"));
		macro_helper(&macros, "#melee", "(19)");
	}

	#[test]
	fn embedded_macro() {
		use macros::*;
		let mut macros = Macros::new();
		macros.insert(String::from("melee"), String::from("[[15+4]]"));
		macro_helper(&macros, "[[ #melee ]]", "(19)");
	}

	#[test]
	fn embedded_inline_roll() {
		helper("/r 10+[[7+8]]", "10 + (15) = 25");
	}
}
