// macros.rs

use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct MacroData {
	pub in_bar: bool,
	pub source: String,
}
impl MacroData {
	pub fn new(in_bar: bool, source: &str) -> MacroData {
		MacroData {
			in_bar: in_bar,
			source: source.to_string(),
		}
	}
}

pub type Macros = HashMap<String, MacroData>;