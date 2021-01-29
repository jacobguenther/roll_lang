// File: lib.rs

extern crate unicode_segmentation;

pub mod ast;
pub mod interpreter;
pub mod lexer;
pub mod macros;
pub mod parser;
pub mod builder;

#[cfg(test)]
pub mod tests {
	use super::interpreter::*;
	use crate::builder::InterpreterBuilder;

	fn r() -> f64 {
		1.0
	}
	fn helper(source: &str, result: &str) {
		let output = InterpreterBuilder::default()
			.with_source(&source)
			.build(r)
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
	}

	#[test]
	fn whitespaces() {
		helper("[[ 20 + 4 * 2 ]]", "(28)");
		helper("[[ 20 + 4 * 2 ]] ", "(28) ");
		helper("/r 20 + 4 * 2 ", "20 + 4 * 2 = 28 ");
		helper("/r 20 + 4 * 2 \\", "20 + 4 * 2 = 28");
		helper("/r 20 + 4 * 2 \\ ", "20 + 4 * 2 = 28 ");
	}

	#[test]
	fn comments() {
		helper("/r [1]20 \\", "[1]20 = 20");
		helper("/r 20[1] \\", "20[1] = 20");
		helper("/r [1]20[2] \\", "[1]20[2] = 20");

		helper("/r [1]1d1 \\", "[1](2) = 2");
		helper("/r 1d1[1] \\", "(2)[1] = 2");
		helper("/r [1]1d1[2] \\", "[1](2)[2] = 2");
	}

	#[test]
	fn tooltips() {
		helper("/r [?1]20 \\", "[20 | tip: 1] = 20");
		helper("/r 20[?1] \\", "[20 | tip: 1] = 20");

		helper("/r [?1]1d1 \\", "[(2) | tip: 1] = 2");
		helper("/r 1d1[?1] \\", "[(2) | tip: 1] = 2");
	}

	#[test]
	fn comments_and_tooltips() {
		helper("/r [1][?2]20 \\", "[1][20 | tip: 2] = 20");
		helper("/r [1]20[?2] \\", "[1][20 | tip: 2] = 20");
		helper("/r 20[?1][2] \\", "[20 | tip: 1][2] = 20");

		helper("/r [1][?2]20[3] \\", "[1][20 | tip: 2][3] = 20");
		helper("/r [1]20[?2][3] \\", "[1][20 | tip: 2][3] = 20");

		helper("/r [1][?2]1d1 \\", "[1][(2) | tip: 2] = 2");
		helper("/r [1]1d1[?2] \\", "[1][(2) | tip: 2] = 2");
		helper("/r 1d1[?1][2] \\", "[(2) | tip: 1][2] = 2");

		helper("/r [1][?2]1d1[3] \\", "[1][(2) | tip: 2][3] = 2");
		helper("/r [1]1d1[?2][3] \\", "[1][(2) | tip: 2][3] = 2");

		helper("/r [1][?2]3d1 \\", "[1][(2+2+2) | tip: 2] = 6");
		helper("/r [1]4d1[?2] \\", "[1][(2+2+2+2) | tip: 2] = 8");
		helper("/r 5d1[?1][2] \\", "[(2+2+2+2+2) | tip: 1][2] = 10");
	}

	#[test]
	fn embedded_inline_roll() {
		helper("/r 10+[[7+8]]", "10 + (15) = 25");
	}

	use super::macros::*;
	fn macro_helper(macros: &Macros, source: &str, result: &str) {
		let interpreter_result = InterpreterBuilder::default()
			.with_source(&source)
			.with_macros(&macros)
			.build(|| 1.0)
			.interpret()
			.to_string();
		assert_eq!(interpreter_result, result);
	}

	#[test]
	fn top_level_macro() {
		let mut macros = Macros::new();
		macros.insert(String::from("melee attack"), String::from("[[15+4]]"));
		macro_helper(&macros, "#{melee attack}", "(19)");
	}
	#[test]
	fn top_level_short_macro() {
		let mut macros = Macros::new();
		macros.insert(String::from("melee"), String::from("[[15+4]]"));
		macro_helper(&macros, "#melee", "(19)");
	}

	#[test]
	fn embedded_macro() {
		let mut macros = Macros::new();
		macros.insert(String::from("melee"), String::from("[[15+4]]"));
		macro_helper(&macros, "[[ #melee ]]", "(19)");
	}
}
