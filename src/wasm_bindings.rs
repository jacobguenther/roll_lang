// File: lib.rs

use crate::builder::*;
use crate::interpreter::output::formats::to_html::*;
use crate::interpreter::*;

use wasm_bindgen::prelude::*;

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

#[wasm_bindgen]
pub fn interpret(source: &str) -> String {
	InterpreterBuilder::default()
		.with_query_prompter(prompt)
		.build(rand_func)
		.interpret(source)
		.to_html()
}

#[wasm_bindgen]
pub fn add(a: i32, b: i32) -> i32 {
	a + b
}
