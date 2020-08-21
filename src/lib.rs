// File: lib.rs

extern crate unicode_segmentation;

mod lexer;
mod parser;
pub mod ast;
pub mod interpreter;

#[cfg(feature = "web")]
pub mod macros;

#[cfg(feature = "web")]
pub mod wasm_interface;