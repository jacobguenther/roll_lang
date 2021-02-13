// File: parser.rs

pub mod error;
use error::ParseError;

mod state;
use state::State;

mod private_traits;
use private_traits::ParserPrivateT;

#[cfg(feature = "serialize")]
use serde::{Deserialize, Serialize};

use super::lexer::{lexeme::Lexeme, Lexer};

use super::ast::*;

pub trait ParserT {
	fn parse(&mut self) -> Root;
	fn parse_expression_string(source: &str) -> Result<Expression, ParseError>;
}

#[derive(Debug)]
pub struct Parser {
	state: State,
	lexemes: Vec<Lexeme>,
	current_index: usize,
}
impl Parser {
	pub fn new(source: &str) -> Parser {
		Parser {
			state: State::default(),
			lexemes: Lexer::new(source).collect(),
			current_index: 0,
		}
	}
}
impl ParserT for Parser {
	fn parse(&mut self) -> Root {
		let mut root = Root::new();
		loop {
			match self.state {
				State::Start => self.parse_start(),
				State::StringLiteral => {
					if let Ok(my_macro) = self.parse_macro() {
						root.push(Node::Macro(my_macro));
						continue;
					}
					let string_literal = self.parse_string_literal();
					root.push(Node::StringLiteral(string_literal));
				}
				State::Roll => match self.parse_roll() {
					Ok(roll) => {
						root.push(Node::Roll(Box::new(roll)));
						self.state = State::Start;
					}
					Err(parse_error) => {
						root.push(Node::ParseError(parse_error.clone()));
						self.state = State::Done;
					}
				},
				State::Done => break,
			};
		}
		root
	}
	fn parse_expression_string(source: &str) -> Result<Expression, ParseError> {
		let mut parser = Parser::new(source);
		parser.state = State::Roll;
		parser.parse_expression()
	}
}
