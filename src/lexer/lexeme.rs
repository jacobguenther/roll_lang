// lexer/lexeme.rs

use super::keywords::Keyword;
use super::token::{Token, TokenT};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Lexeme {
	Whitespace(Token),
	Literal(Token),
	Keyword(Token, Keyword),
	Number(Token),
	Comparison(Token),
	Operator(Token),
	Punctuation(Token),
}
impl Lexeme {
	pub fn token(&self) -> &Token {
		match self {
			Lexeme::Whitespace(t)
			| Lexeme::Literal(t)
			| Lexeme::Keyword(t, _)
			| Lexeme::Number(t)
			| Lexeme::Comparison(t)
			| Lexeme::Operator(t)
			| Lexeme::Punctuation(t) => t,
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
	fn truncate(&mut self, size: usize) {
		match self {
			Lexeme::Whitespace(t)
			| Lexeme::Literal(t)
			| Lexeme::Keyword(t, _)
			| Lexeme::Number(t)
			| Lexeme::Comparison(t)
			| Lexeme::Operator(t)
			| Lexeme::Punctuation(t) => t.truncate(size),
		}
	}
	fn push_str(&mut self, s: &str) {
		match self {
			Lexeme::Whitespace(t)
			| Lexeme::Literal(t)
			| Lexeme::Keyword(t, _)
			| Lexeme::Number(t)
			| Lexeme::Comparison(t)
			| Lexeme::Operator(t)
			| Lexeme::Punctuation(t) => t.push_str(s),
		}
	}
}
