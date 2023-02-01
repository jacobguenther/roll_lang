// File: lexer/mod.rs

pub mod keywords;
pub mod lexeme;
mod state;
pub mod token;
mod trie;

use crate::ast::Comparison;
use lexeme::Lexeme;
use state::State;
use token::{Token, TokenT};

use std::convert::TryFrom;

use keywords::Keyword;

use trie::*;
use unicode_segmentation::{
	Graphemes,
	UnicodeSegmentation
};

static INIT_KEYWORDS_TRIE: std::sync::Once = std::sync::Once::new();
static mut KEYWORDS_TRIE: Option<Trie<ALPABET_SIZE>> = None;
pub fn keywords_trie() -> &'static mut Trie<ALPABET_SIZE> {
	unsafe {
		INIT_KEYWORDS_TRIE.call_once(|| {
			KEYWORDS_TRIE = Some(Trie::default());
		});
		KEYWORDS_TRIE.as_mut().unwrap()
	}
}

#[derive(Debug)]
pub struct Lexer<'a> {
	graphemes: Graphemes<'a>,
	current: Option<&'a str>,

	previous_state: State,
	state: State,
	current_index: usize,
}

impl<'a> Lexer<'a> {
	pub fn new(source: &str) -> Lexer {
		let mut lexer = Lexer {
			graphemes: UnicodeSegmentation::graphemes(source, true),
			current: None,

			previous_state: State::Start,
			state: State::Start,
			current_index: 0,
		};
		lexer.advance();
		lexer
	}
}
impl<'a> Iterator for Lexer<'a> {
	type Item = Lexeme;
	fn next(&mut self) -> Option<Self::Item> {
		if self.at_end() {
			return None;
		}

		let mut lexeme_token = Token::new(self.current_index);

		self.update_state(State::Start);
		loop {
			self.handle_digit(&mut lexeme_token);
			if self.state == State::Done {
				break;
			}
			self.handle_operator(&mut lexeme_token);
			if self.state == State::Done {
				break;
			}
			self.handle_comparison(&mut lexeme_token);
			if self.state == State::Done {
				break;
			}
			self.handle_whitespace(&mut lexeme_token);
			if self.state == State::Done {
				break;
			}
			if let Some(c) = self.current() {
				if Lexer::is_punctuation(c) {
					self.update_state(State::Punctuation);
					self.push_current_on(&mut lexeme_token);
					self.advance();
					self.update_state(State::Done);
					break;
				}
			}
			self.handle_keyword(&mut lexeme_token);
			if self.state == State::Done {
				break;
			}
		}

		match self.previous_state {
			State::Start => None,
			State::Done => None,
			State::Whitespace => Some(Lexeme::Whitespace(lexeme_token)),
			State::Keyword => {
				let keyword = Keyword::try_from(lexeme_token.source()).unwrap();
				Some(Lexeme::Keyword(lexeme_token, keyword))
			}
			State::Literal => Some(Lexeme::Literal(lexeme_token)),
			State::Integer => {
				let int = lexeme_token.source().parse().unwrap();
				Some(Lexeme::Integer(lexeme_token, int))
			}
			State::Float => {
				let float = lexeme_token.source().parse().unwrap();
				Some(Lexeme::Float(lexeme_token, float))
			}
			State::Comparison => {
				let cmp = Comparison::try_from(lexeme_token.source()).unwrap();
				Some(Lexeme::Comparison(lexeme_token, cmp))
			}
			State::Operator => Some(Lexeme::Operator(lexeme_token)),
			State::Punctuation => Some(Lexeme::Punctuation(lexeme_token)),
		}
	}
}

impl<'a> Lexer<'a> {
	fn current(&self) -> Option<&str> {
		self.current
	}
	fn current_unchecked(&self) -> &str {
		self.current.unwrap()
	}

	fn at_end(&self) -> bool {
		self.current.is_none()
	}
	fn advance(&mut self) {
		self.current_index += 1;
		self.current = self.graphemes.next();
	}
	fn push_current_on(&mut self, lexeme_token: &mut Token) {
		lexeme_token.push_str(self.current_unchecked());
	}
	fn update_state(&mut self, new: State) {
		self.previous_state = self.state;
		self.state = new;
	}

	fn handle_digit(&mut self, lexeme_token: &mut Token) {
		self.template_function_match_repeated(Lexer::is_digit, lexeme_token, State::Integer);
		if self.current() == Some(".") {
			self.push_current_on(lexeme_token);
			self.advance();
			self.template_function_match_repeated(Lexer::is_digit, lexeme_token, State::Float);
		}
	}
	fn handle_whitespace(&mut self, lexeme_token: &mut Token) {
		self.template_function_match_repeated(Lexer::is_whitespace, lexeme_token, State::Whitespace);
	}
	fn handle_comparison(&mut self, lexeme_token: &mut Token) {
		match self.current() {
			Some("<") | Some(">") => {
				self.update_state(State::Comparison);
				self.push_current_on(lexeme_token);
				self.advance();
				if self.current() == Some("=") {
					self.push_current_on(lexeme_token);
					self.advance();
				}
			}
			Some("=") => {
				self.update_state(State::Comparison);
				self.push_current_on(lexeme_token);
				self.advance();
			}
			_ => {
				self.update_state(State::Start);
				return;
			},
		}
		self.update_state(State::Done);
	}
	fn handle_operator(&mut self, lexeme_token: &mut Token) {
		match self.current() {
			Some("*") => {
				self.update_state(State::Operator);
				self.push_current_on(lexeme_token);
				self.advance();
				if self.current() == Some("*") {
					self.push_current_on(lexeme_token);
					self.advance();
				}
				self.update_state(State::Done);
			}
			Some("!") => {
				self.update_state(State::Operator);
				self.push_current_on(lexeme_token);
				self.advance();
				if matches!(self.current(), Some("!") | Some("p")) {
					self.push_current_on(lexeme_token);
					self.advance();
				}
				self.update_state(State::Done);
			}
			Some("/") | Some("+") | Some("-") | Some("%") | Some("^") => {
				self.update_state(State::Operator);
				self.push_current_on(lexeme_token);
				self.advance();
				self.update_state(State::Done);
			}
			_ => {
				self.update_state(State::Start);
			}
		}
	}
	fn handle_keyword(&mut self, lexeme_token: &mut Token) {
		let mut iter = self.graphemes.clone();

		if self.current().is_some() {
			self.push_current_on(lexeme_token);
		} else {
			self.update_state(State::Done);
			return;
		}

		let mut exact_len = 0;
		let mut advanced_by = 0;

		let mut found_exact = false;
		let mut last_prefix_len = 0;
		let mut matched_upto_id = None;

		while let Some(m) = keywords_trie()
			.is_match_from(&lexeme_token.source()[last_prefix_len..], matched_upto_id)
		{
			match m {
				TrieMatch::Prefix(id) => {
					last_prefix_len = lexeme_token.source().len();
					matched_upto_id = Some(id);
				}
				TrieMatch::Exact(id) => {
					last_prefix_len = lexeme_token.source().len();
					matched_upto_id = Some(id);

					found_exact = true;
					exact_len = advanced_by + 1;
				}
				TrieMatch::ExactLongest => {
					self.update_state(State::Keyword);
					for _ in 0..advanced_by + 1 {
						self.advance();
					}
					self.update_state(State::Done);
					return;
				}
			}

			match iter.next() {
				Some(c) => {
					advanced_by += 1;
					lexeme_token.push_str(c);
				}
				None => break,
			};
		}

		if found_exact {
			lexeme_token.truncate(exact_len);
			self.update_state(State::Keyword);
			for _ in 0..exact_len {
				self.advance();
			}
			self.update_state(State::Done);
		} else {
			for _ in 0..advanced_by + 1 {
				self.advance();
			}
			self.handle_literal(lexeme_token);
			self.update_state(State::Literal);
			self.update_state(State::Done);
		}
	}
	fn handle_literal(&mut self, lexeme_token: &mut Token) {
		self.template_function_match_repeated(Lexer::is_literal, lexeme_token, State::Literal);
	}

	fn is_literal(c: &str) -> bool {
		!Lexer::is_digit(c)
			&& !Lexer::is_whitespace(c)
			&& !Lexer::is_operator(c)
			&& !Lexer::is_punctuation(c)
			&& !Lexer::is_comparison_operator(c)
	}
	// invariant: s contains only 1 grapheme
	fn is_whitespace(s: &str) -> bool {
		matches!(s, " " | "\t" | "\n")
	}

	// invariant: s contains only 1 grapheme
	fn is_digit(s: &str) -> bool {
		matches!(s, "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9")
	}

	fn is_comparison_operator(s: &str) -> bool {
		matches!(s, "<" | ">" | "=" | "<=" | ">=")
	}
	fn is_operator(s: &str) -> bool {
		matches!(
			s,
			"+" | "-" | "*" | "/" | "!" | "!!" | "!p" | "%" | "**" | "^"
		)
	}
	fn is_punctuation(s: &str) -> bool {
		matches!(
			s,
			"[" | "]" | "(" | ")" | "{" | "}" | "\\" | "." | "?" | "|" | "#"
		)
	}

	fn template_function_match_repeated(
		&mut self,
		check: fn(&str) -> bool,
		lexeme_token: &mut Token,
		state: State,
	) {
		let mut found_one = false;
		while let Some(c) = self.current() {
			if check(c) {
				found_one = true;
				self.push_current_on(lexeme_token);
				self.advance();
			} else {
				break;
			}
		}
		if found_one {
			self.update_state(state);
			self.update_state(State::Done);
		}
	}
}
