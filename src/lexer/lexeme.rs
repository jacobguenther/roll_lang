// lexer/lexeme.rs

use super::token::{Token, TokenT};

#[derive(Debug, Copy, Clone, PartialEq)]
pub(super) enum LexemeType {
	Whitespace,
	Literal,
	Keyword,
	Number,
	Comparison,
	Operator,
	Punctuation,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lexeme {
	Whitespace(Token),
	Literal(Token),
	Keyword(Token),
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
			| Lexeme::Keyword(t)
			| Lexeme::Number(t)
			| Lexeme::Comparison(t)
			| Lexeme::Operator(t)
			| Lexeme::Punctuation(t) => t,
		}
	}
	pub(super) fn into(&mut self, lexeme: &LexemeType) -> Lexeme {
		match lexeme {
			LexemeType::Whitespace => Lexeme::Whitespace(self.token().clone()),
			LexemeType::Literal => Lexeme::Literal(self.token().clone()),
			LexemeType::Keyword => Lexeme::Keyword(self.token().clone()),
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
	fn truncate(&mut self, size: usize) {
		match self {
			Lexeme::Whitespace(t)
			| Lexeme::Literal(t)
			| Lexeme::Keyword(t)
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
			| Lexeme::Keyword(t)
			| Lexeme::Number(t)
			| Lexeme::Comparison(t)
			| Lexeme::Operator(t)
			| Lexeme::Punctuation(t) => t.push_str(s),
		}
	}
}
