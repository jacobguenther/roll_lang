// lexer/lexeme.rs

use super::token::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Lexeme {
	Whitespace(Token),
	Literal(Token),
	Number(Token),
	Comparison(Token),
	Operator(Token),
	Punctuation(Token),
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