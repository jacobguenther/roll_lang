// lexer/lexeme.rs

use super::keywords::Keyword;
use super::token::{
	Token,
	TokenT,
};
use crate::ast::{
	number::*,
	Comparison,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Lexeme {
	Whitespace(Token),
	Literal(Token),
	Keyword(Token, Keyword),
	Integer(Token, Integer),
	Float(Token, Float),
	Comparison(Token, Comparison),
	Operator(Token),
	Punctuation(Token),
}
impl Lexeme {
	pub fn token(&self) -> &Token {
		match self {
			Lexeme::Whitespace(t)
			| Lexeme::Literal(t)
			| Lexeme::Keyword(t, _)
			| Lexeme::Integer(t, _)
			| Lexeme::Float(t, _)
			| Lexeme::Comparison(t, _)
			| Lexeme::Operator(t)
			| Lexeme::Punctuation(t) => t,
		}
	}
	pub fn is_whitespace(&self) -> bool {
		matches!(self, Lexeme::Whitespace(_t))
	}
	pub fn is_literal(&self) -> bool {
		matches!(self, Lexeme::Literal(_t))
	}
	pub fn is_keyword(&self) -> bool {
		matches!(self, Lexeme::Keyword(_t, _keyword))
	}
	pub fn is_integer(&self) -> bool {
		matches!(self, Lexeme::Integer(_t, _i))
	}
	pub fn is_float(&self) -> bool {
		matches!(self, Lexeme::Integer(_t, _f))
	}
	pub fn is_comparison(&self) -> bool {
		matches!(self, Lexeme::Comparison(_t, _comparison))
	}
	pub fn is_operator(&self) -> bool {
		matches!(self, Lexeme::Operator(_t))
	}
	pub fn is_punctuation(&self) -> bool {
		matches!(self, Lexeme::Punctuation(_t))
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
			| Lexeme::Integer(t, _)
			| Lexeme::Float(t, _)
			| Lexeme::Comparison(t, _)
			| Lexeme::Operator(t)
			| Lexeme::Punctuation(t) => t.truncate(size),
		}
	}
	fn push_str(&mut self, s: &str) {
		match self {
			Lexeme::Whitespace(t)
			| Lexeme::Literal(t)
			| Lexeme::Keyword(t, _)
			| Lexeme::Integer(t, _)
			| Lexeme::Float(t, _)
			| Lexeme::Comparison(t, _)
			| Lexeme::Operator(t)
			| Lexeme::Punctuation(t) => t.push_str(s),
		}
	}
}
