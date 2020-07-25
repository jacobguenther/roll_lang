// File: lib.rs

extern crate unicode_segmentation;

pub mod lexer;
pub mod parser;
pub mod interpreter;

use lexer::{
    Lexer,
    LexerT,
};
use parser::{
    Parser,
    ParserT,
};

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

#[wasm_bindgen]
pub fn run(source: &str) -> String {
    let mut lexer = Lexer::new(source);
    let mut out = String::new();
    while let Some(lexeme) = lexer.next() {
        out = format!("{}<br>{:?}", out, lexeme);
    }
    let mut parser = Parser::new(source);
    let ast = parser.parse();
    out = format!("{}<br><br>{:?}", out, ast);
    out
}