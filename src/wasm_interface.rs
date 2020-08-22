// File: wasm_interface.rs

use super::interpreter::{
	Interpreter,
	InterpreterT,
};

use super::macros::{
	*
};

use wasm_bindgen::prelude::*;
use lazy_static::lazy_static;
use std::sync::Mutex;

lazy_static! {
	static ref ARRAY: Mutex<Macros> = Mutex::new(Macros::init());
}

#[wasm_bindgen]
pub fn run(source: &str) -> String {
	let macros = ARRAY.lock().unwrap().clone();
	Interpreter::new(source, &macros)
		.interpret()
		.as_html()
}

#[wasm_bindgen]
pub fn init_panic_hook() {
	console_error_panic_hook::set_once();
}

#[wasm_bindgen]
pub fn init() {
	init_panic_hook();
	let _dummy = ARRAY.lock().unwrap();
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
