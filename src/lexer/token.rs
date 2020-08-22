// File: lexer/token.rs

pub trait TokenT {
	fn source(&self) -> &str;
	fn start(&self) -> usize;
	fn end(&self) -> usize;
	fn length(&self) -> usize;
	fn push_str(&mut self, s: &str);
}
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
	source: String,
	start: usize,
}
impl Token {
	pub fn new(start: usize) -> Token {
		Token {
			source: String::new(),
			start: start,
		}
	}
}
impl TokenT for Token {
	fn source(&self) -> &str {
		&self.source
	}
	fn start(&self) -> usize {
		self.start
	}
	fn end(&self) -> usize {
		self.start + self.source.len()
	}
	fn length(&self) -> usize {
		self.source.len()
	}
	fn push_str(&mut self, s: &str) {
		self.source = format!("{}{}", self.source, s);
	}
}