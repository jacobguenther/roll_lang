// File: lib.rs

extern crate unicode_segmentation;

pub mod lexer;
pub mod parser;
pub mod ast;
pub mod interpreter;
pub mod macros;

use std::collections::HashMap;
use interpreter::*;

pub fn default_rand() -> f64 {
	use rand::{thread_rng, Rng};
	thread_rng().gen()
}

pub fn default_query_prompter(message: &str, default: &str) -> Option<String> {
	use std::io;
	println!("{} default({})", message, default);
	let mut input = String::new();
	match io::stdin().read_line(&mut input) {
		Ok(_) => {
			let input = input.trim().to_owned();
			if &input == "" {
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
	rand: Option<fn()->f64>,
	query_prompter: Option< fn(&str, &str)->Option<String> >
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

	pub fn with_source<'a>(&'a mut self, source: &'s str) -> &'a mut InterpreterBuilder<'s, 'r, 'm> {
		self.source = Some(source);
		self
	}
	pub fn with_roll_queries<'a>(&'a mut self, roll_queries: &'r HashMap<String, ast::Expression>) -> &'a mut InterpreterBuilder<'s, 'r, 'm> {
		self.roll_queries = Some(roll_queries);
		self
	}
	pub fn with_macros<'a>(&'a mut self, macros: &'m macros::Macros) -> &'a mut InterpreterBuilder<'s, 'r, 'm> {
		self.macros = Some(macros);
		self
	}
	pub fn with_rng_func<'a>(&'a mut self, rand: fn()->f64) -> &'a mut InterpreterBuilder<'s, 'r, 'm> {
		self.rand = Some(rand);
		self
	}
	pub fn with_query_prompter<'a>(&'a mut self, prompter: fn(&str, &str) -> Option<String>) -> &'a mut InterpreterBuilder<'s, 'r, 'm> {
		self.query_prompter = Some(prompter);
		self
	}

	// pub fn without_source<'a>(&'a mut self) -> &'a mut InterpreterBuilder<'s, 'r, 'm> {
	// 	self.source = None;
	// 	self
	// }
	// pub fn without_roll_queries<'a>(&'a mut self) -> &'a mut InterpreterBuilder<'s, 'r, 'm> {
	// 	self.roll_queries = None;
	// 	self
	// }
	// pub fn without_macros<'a>(&'a mut self) -> &'a mut InterpreterBuilder<'s, 'r, 'm> {
	// 	self.macros = None;
	// 	self
	// }

	pub fn build(&self) -> Interpreter<'s, 'm> {
		Interpreter::new(
			self.source.unwrap_or(""),
			self.roll_queries.unwrap_or(&HashMap::new()).clone(),
			self.macros,
			self.rand.unwrap_or(default_rand),
			self.query_prompter.unwrap_or(default_query_prompter),
		)
	}
}


#[cfg(test)]
pub mod tests {
	use super::*;
	use super::interpreter::output_traits::*;

	fn helper(
		source: &str,
		result: &str) 
	{
		let output = InterpreterBuilder::new()
			.with_source(&source)
			.build()
			.interpret()
			.to_string();
		assert_eq!(&output, result);
	}
	
	#[test]
    fn interpreter() {
		// associativity
		helper("[[5-4+1]]", "5-4+1=2");
		// multiply and divide
		helper("[[4*6/3]]", "4*6/3=8");
		// precedence
		helper("[[(4+2)*2]]", "(4+2)*2=12");

		// unicode and localization
		helper("文字 hello", "文字 hello");

		// whitespaces
		helper(
			"[[ 20 + 4 * 2 ]]",
			"20+4*2=28");
		// trailing whitespaces
		helper(
			"attack is [[20+1]] and damage is /r 10 \\ take that!",
			"attack is 20+1=21 and damage is 10=10 take that!");
		helper(
			"/r 20*2 is my attack roll",
			"20*2=40 is my attack roll");
		helper(
			"/r 20*2",
			"20*2=40");
		helper(
			"/r 20*2\\ is my attack roll",
			"20*2=40 is my attack roll");
	}
	#[test]
	fn roll_queries() {
		let source = String::from("I attack you for ?{attack|3}");
		helper(
			"I attack you for [[?{attack|3}]]",
			"I attack you for 3=3");
	}
	#[test]
	fn interpreter_builder() {
		use macros::*;

		let source = String::from("I attack you for #attack and deal [[10/2]] damage!");
		let mut macros = Macros::new();
		macros.insert(String::from("attack"), MacroData::new(false, "[[15+4]]"));
		let mut builder = InterpreterBuilder::new();

		{
			let mut interpreter = builder
				.with_source(&source)
				.with_macros(&macros)
				.build();
			let mut interpreter2 = builder.build();
			assert_eq!(
				interpreter.interpret().to_string(),
				String::from("I attack you for 15+4=19 and deal 10/2=5 damage!"));

			assert_eq!(interpreter2.interpret().to_string(), interpreter.interpret().to_string());
		}
	}
	#[test]
	fn rng() {
		use super::default_rand;
		let rand = default_rand();
		assert!(0.0 <= rand && rand <= 1.0);
	}

	#[cfg(feature = "serialize")]
	#[test]
	fn serialize() {
		use interpreter::output::Output;
		let source = String::from("I attack you for /r 20+3[STR] and deal [[10/2]] damage!");

		let out1 = InterpreterBuilder::new()
			.with_source(&source)
			.build()
			.interpret();
		let ser = serde_json::to_string(&out1).unwrap();
		let out2: Output = serde_json::from_str(&ser).unwrap();

		assert_eq!(out1.to_string(), out2.to_string());
	}
}