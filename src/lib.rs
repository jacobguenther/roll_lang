// File: lib.rs

extern crate unicode_segmentation;

pub mod lexer;
pub mod parser;
pub mod ast;
pub mod interpreter;
pub mod macros;

use lexer::{
	Lexer,
	LexerT,
};
use parser::{
	Parser,
	ParserT,
};
use interpreter::{
	Interpreter,
	InterpreterT,
};
use macros::{
	*
};

use wasm_bindgen::prelude::*;

use lazy_static::lazy_static;
use std::sync::Mutex;

lazy_static! {
	static ref ARRAY: Mutex<Macros> = Mutex::new(Macros::init());
}

#[wasm_bindgen]
extern "C" {
	#[wasm_bindgen(js_namespace = console)]
	fn log(s: &str);

	#[wasm_bindgen(js_namespace = Math, js_name = random)]
	fn random() -> i32;


	#[wasm_bindgen(js_namespace = window, js_name = prompt)]
	fn prompt(message: &str, default: &str) -> String;
}


#[wasm_bindgen]
pub fn init_panic_hook() {
	console_error_panic_hook::set_once();
}

#[wasm_bindgen]
pub fn run(source: &str) -> String {
	Interpreter::new(source)
		.interpret()
		.as_html()
}

 
#[wasm_bindgen]
pub fn init() {
	ARRAY.lock().unwrap();
}
#[wasm_bindgen]
pub fn handle_macro_update_create() {
	ARRAY.lock().unwrap().handle_macro_update_create();
}
#[wasm_bindgen]
pub fn handle_macro_delete(name: &str) {
	ARRAY.lock().unwrap().handle_macro_delete(name);
}

#[wasm_bindgen]
pub fn handle_macro_select(name: &str) {
	ARRAY.lock().unwrap().handle_macro_select(name);
}
#[wasm_bindgen]
pub fn handle_macro_change_in_bar(name: &str) {
	ARRAY.lock().unwrap().handle_macro_change_in_bar(name);
}

#[wasm_bindgen]
pub fn macro_source(name: &str) -> Option<String> {
	ARRAY.lock().unwrap().source(name)
}