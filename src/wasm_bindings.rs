// File: lib.rs
// Author: Jacob Guenther(chmod777)
// License: AGPLv3

/*
wasm bindings can be enabled in with the feature "wasm-bindings".
*/

use crate::builder::*;
use crate::interpreter::output::formats::to_html::*;
use crate::interpreter::*;

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn interpret(source: &str) -> String {
	InterpreterBuilder::default()
		.with_query_prompter(prompt)
		.build(rand_func)
		.interpret(source)
		.to_html()
}

fn rand_func() -> f64 {
	use js_sys::Math::random;
	random()
}

fn prompt(message: &str, default: &str) -> Option<String> {
	web_sys::window()
		.unwrap()
		.prompt_with_message_and_default(message, default)
		.unwrap_or(None)
}
