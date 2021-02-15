// File: tests/macros.rs

use super::*;
use crate::macros::*;

fn macro_helper<R: Copy + Fn() -> f64>(
	random_func: R,
	macros: &Macros,
	source: &str,
	result: &str,
) {
	let interpreter_result = InterpreterBuilder::default()
		.with_source(&source)
		.with_macros(&macros)
		.build(random_func)
		.interpret()
		.to_string();
	assert_eq!(interpreter_result, result);
}

#[test]
fn top_level_macro() {
	let mut macros = Macros::new();
	macros.insert(String::from("melee attack"), String::from("[[15+4]]"));
	macro_helper(r, &macros, "#{melee attack}", "(19)");
}
#[test]
fn top_level_short_macro() {
	let mut macros = Macros::new();
	macros.insert(String::from("melee"), String::from("[[15+4]]"));
	macro_helper(r, &macros, "#melee", "(19)");
}

#[test]
fn embedded_macro() {
	let mut macros = Macros::new();
	macros.insert(String::from("melee"), String::from("[[15+4]]"));
	macro_helper(r, &macros, "[[ #melee ]]", "(19)");
}
