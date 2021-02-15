// File: tests/mod.rs

pub mod comments_tooltips;
pub mod functions;
pub mod macros;
pub mod modifires;

use output::Output;

use crate::builder::InterpreterBuilder;
use crate::interpreter::*;

use crate::interpreter::error::InterpretError;
use crate::parser::error::ParseError;

pub fn r() -> f64 {
	0.0
}
fn helper<R: Copy + Fn() -> f64>(random_func: R, source: &str, result: &str) {
	let output = InterpreterBuilder::default()
		.with_source(&source)
		.build(random_func)
		.interpret()
		.to_string();
	println!("{}", source);
	assert_eq!(&output, result);
}
fn helper_return_result<R: Copy + Fn() -> f64>(random_func: R, source: &str) -> Output {
	InterpreterBuilder::default()
		.with_source(&source)
		.build(random_func)
		.interpret()
}

#[test]
fn playground() {
	println!(
		"{}",
		InterpreterBuilder::default()
			.with_source(
				"I deal /r d6[spear] + [[ 3[str mod] + 4[prof bonus] ]][modifiers] \\ damage"
			)
			.build(r)
			.interpret()
			.to_string()
	);
	// assert!(false);
}

#[test]
fn interpreter() {
	// associativity
	helper(r, "[[5-4+1]]", "(2)");
	// multiply and divide
	helper(r, "[[4*6/3]]", "(8)");
	// precedence
	helper(r, "[[(4+2)*2]]", "(12)");
	// unicode and localization
	helper(r, "文字 hello", "文字 hello");
}

#[test]
fn whitespaces() {
	helper(r, "[[ 20 + 4 * 2 ]]", "(28)");
	helper(r, "[[ 20 + 4 * 2 ]] ", "(28) ");
	helper(r, "/r 20 + 4 * 2 ", "20 + 4 * 2 = 28 ");
	helper(r, "/r 20 + 4 * 2 \\", "20 + 4 * 2 = 28");
	helper(r, "/r 20 + 4 * 2 \\ ", "20 + 4 * 2 = 28 ");
}

#[test]
fn embedded_inline_roll() {
	helper(r, "/r 10+[[7+8]]", "10 + (15) = 25");
}
