// File: tests/mod.rs

pub mod comments_tooltips;
pub mod drop_keep;
pub mod functions;
pub mod macros;
pub mod modifires;
pub mod sort;

use output::Output;

use crate::builder::InterpreterBuilder;
use crate::interpreter::*;

use crate::interpreter::error::InterpretError;
use crate::parser::error::ParseError;

pub fn r() -> f64 {
	0.0
}
fn helper<R: Copy + Fn() -> f64>(random_func: R, source: &str, expected_output: &str) {
	let output = InterpreterBuilder::default()
		.with_source(&source)
		.build(random_func)
		.interpret()
		.to_string();
	println!("{}", source);
	assert_eq!(&output, expected_output);
}
fn helper_return_result<R: Copy + Fn() -> f64>(random_func: R, source: &str) -> Output {
	InterpreterBuilder::default()
		.with_source(&source)
		.build(random_func)
		.interpret()
}

// #[test]
#[allow(unused)]
fn playground() {
	let r = || -> f64 {
		let nums = [0.0, 0.1, 0.3, 0.4];
		static mut I: usize = 0;
		unsafe {
			let rand = nums[I];
			I += 1;
			if I >= 4 {
				I = 0;
			}
			rand
		}
	};
	println!(
		"{}",
		InterpreterBuilder::default()
			.with_source("/r 4d10kl2")
			.build(r)
			.interpret()
			.to_string()
	);
	assert!(false);
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
