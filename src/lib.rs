// File: lib.rs
// Author: Jacob Guenther(chmod777)
// License: AGPLv3

/*
Documentation for the project can be found here TODO
*/

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
