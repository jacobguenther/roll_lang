// File: lib.rs

extern crate unicode_segmentation;

pub mod lexer;
pub mod parser;
pub mod ast;
pub mod interpreter;

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

#[macro_use]
extern crate stdweb;

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
	#[wasm_bindgen(js_namespace = console)]
	fn log(s: &str);

	#[wasm_bindgen(js_namespace = Math, js_name = random)]
	fn random() -> i32;
}


#[wasm_bindgen]
pub fn init_panic_hook() {
	console_error_panic_hook::set_once();
}

#[wasm_bindgen]
pub fn run(source: &str) -> String {
	if false {
		let mut out = String::new();

		if true {
			let mut lexer = Lexer::new(source);
			while let Some(lexeme) = lexer.next() {
				out = format!("{}{:?}", out, lexeme);
			}
		}
		
		if true {
			let mut parser = Parser::new(source);
			let ast = parser.parse();
			out = format!("{}<br><br>{:?}", out, ast);
		}
		
		let interpreter = Interpreter::new(source);
		format!("{}<br><br>{}",
			out,
			interpreter.interpret().as_html()
		)
	} else {
		Interpreter::new(source)
			.interpret()
			.as_html()
	}
}