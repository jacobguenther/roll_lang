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
		.with_macros(&macros)
		.build(random_func)
		.interpret(&source)
		.to_string();
	println!("input: {}", source);
	assert_eq!(interpreter_result, result);
}

#[test]
fn errros() {
	let mut macros = Macros::new();

	macro_helper(r, &macros, "#", "::NoMacroNamed(\"\")");
	macro_helper(r, &macros, "# ", "::NoMacroNamed(\"\")");
	macro_helper(r, &macros, "#{}", "::NoMacroNamed(\"\")");

	macro_helper(r, &macros, "#test", "::NoMacroNamed(\"test\")");
	macro_helper(r, &macros, "#range", "::NoMacroNamed(\"range\")");

	macros.insert("test".to_owned(), "[[ 1 ]]".to_owned());
	macro_helper(r, &macros, "random text #test", "random text (1)");
}

#[test]
fn long_macro() {
	let mut macros = Macros::new();
	macros.insert("melee attack".to_owned(), "[[15+4]]".to_owned());
	macros.insert("range attack".to_owned(), "12+4".to_owned());

	macro_helper(r, &macros, "#{melee attack}", "(19)");
	macro_helper(r, &macros, "[[ #{melee attack} ]]", "(19)");
	macro_helper(
		r,
		&macros,
		"/r #{melee attack}",
		"[{(19)} | tip: #{melee attack}] = 19",
	);

	// macro_helper(r, &macros, "#{range attack}", "12+4");
	// macro_helper(r, &macros, "[[ #{range attack} ]]", "(16)");
	// macro_helper(r, &macros, "/r #{range attack}", "{12 + 4} = 16");
}

#[test]
fn short_macro() {
	let mut macros = Macros::new();
	macros.insert("melee".to_owned(), "[[15+4]]".to_owned());
	macros.insert("range".to_owned(), "12+4".to_owned());

	macro_helper(r, &macros, "#melee", "(19)");
	macro_helper(r, &macros, "[[ #melee ]]", "(19)");
	macro_helper(r, &macros, "/r #melee", "[{(19)} | tip: #melee] = 19");

	// macro_helper(r, &macros, "#range", "12+4");
	// macro_helper(r, &macros, "[[ #range ]]", "(16)");
	// macro_helper(r, &macros, "/r #range", "{12 + 4} = 16");
}

#[test]
fn nested_macro() {
	let mut macros = Macros::new();
	macros.insert("melee".to_owned(), "[[15+4]]".to_owned());
	macros.insert("range".to_owned(), "[[12+4]]".to_owned());
	macros.insert("both".to_owned(), "#melee #range".to_owned());

	macro_helper(r, &macros, "#both", "(19) (16)");
}
