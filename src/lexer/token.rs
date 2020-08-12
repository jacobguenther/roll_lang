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

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum LexemeType {
	Whitespace,
	Literal,
	Number,
	Comparison,
	Operator,
	Punctuation,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lexeme {
	Whitespace(Token),
	Literal(Token),
	Number(Token),
	Comparison(Token),
	Operator(Token),
	Punctuation(Token),
}
impl Lexeme {
	pub fn mut_token(&mut self) -> &mut Token {
		match self {
			Lexeme::Whitespace(t) | Lexeme::Literal(t) | Lexeme::Number(t) |
			Lexeme::Comparison(t) | Lexeme::Operator(t) | Lexeme::Punctuation(t) => t 
		}
	}
	pub fn token(&self) -> &Token {
		match self {
			Lexeme::Whitespace(t) | Lexeme::Literal(t) | Lexeme::Number(t) |
			Lexeme::Comparison(t) | Lexeme::Operator(t) | Lexeme::Punctuation(t) => t 
		}
	}
	pub(super) fn into(&mut self, lexeme: &LexemeType) -> Lexeme {
		match lexeme {
			LexemeType::Whitespace => Lexeme::Whitespace(self.token().clone()),
			LexemeType::Literal => Lexeme::Literal(self.token().clone()),
			LexemeType::Number => Lexeme::Number(self.token().clone()),
			LexemeType::Comparison => Lexeme::Comparison(self.token().clone()),
			LexemeType::Operator => Lexeme::Operator(self.token().clone()),
			LexemeType::Punctuation => Lexeme::Punctuation(self.token().clone()),
		}
	}
}
impl TokenT for Lexeme {
	fn source(&self) -> &str {
		self.token().source()
	}
	fn start(&self) -> usize {
		self.token().start()
	}
	fn end(&self) -> usize {
		self.token().end()
	}
	fn length(&self) -> usize {
		self.token().length()
	}
	fn push_str(&mut self, s: &str) {
		self.mut_token().push_str(s);
	}
}