// File: lib.rs

extern crate unicode_segmentation;

pub mod ast;
pub mod builder;
pub mod interpreter;
pub mod lexer;
pub mod macros;
pub mod parser;

#[cfg(feature = "wasm-bindings")]
pub mod wasm_bindings;

#[cfg(test)]
pub mod tests;
