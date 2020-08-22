// File: lib.rs

extern crate unicode_segmentation;

pub mod lexer;
pub mod parser;
pub mod ast;
pub mod interpreter;
pub mod macros;

#[cfg(test)]
pub mod tests {
	use std::collections::HashMap;
	use super::*;
	use super::interpreter::*;
	use super::interpreter::output_traits::*;
	use super::macros::MacroData;

	fn helper(source: &str, result: &str, macros: &HashMap<String, MacroData>) {
		let output = interpreter::Interpreter::new(&source, &macros)
			.interpret()
			.as_string();
		assert_eq!(&output, result);
	}
	
	#[test]
    fn interpreter() {
		// associativity
		helper("[[5-4+1]]", "5-4+1=2", &HashMap::new());
		// multiply and divide
		helper("[[4*6/3]]", "4*6/3=8", &HashMap::new());
		// precedence
		helper("[[(4+2)*2]]", "(4+2)*2=12", &HashMap::new());

		// unicode and localization
		helper("文字 hello", "文字 hello", &HashMap::new());

		// whitespaces
		helper(
			"[[ 20 + 4 * 2 ]]",
			"20+4*2=28",
			&HashMap::new());
		// trailing whitespaces
		helper(
			"attack is [[20+1]] and damage is /r 10 \\ take that!",
			"attack is 20+1=21 and damage is 10=10 take that!",
			&HashMap::new());
		helper(
			"/r 20*2 is my attack roll",
			"20*2=40 is my attack roll",
			&HashMap::new());
		helper(
			"/r 20*2\\ is my attack roll",
			"20*2=40 is my attack roll",
			&HashMap::new());
	}
}