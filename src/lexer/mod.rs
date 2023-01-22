// File: lexer/mod.rs

pub mod keywords;
pub mod lexeme;
mod state;
pub mod token;
pub mod trie;

use lexeme::Lexeme;
use state::State;
use token::{Token, TokenT};
use crate::ast::Comparison;

use std::convert::TryFrom;

use keywords::{Keyword, KEYWORDS};

// use trie_rs::Trie;
// use trie_rs::TrieBuilder;
use trie::*;
use unicode_segmentation::UnicodeSegmentation;

#[derive(Debug)]
pub struct Lexer<'a> {
	keywords_trie: Trie,
	graphemes: Vec<&'a str>,
	previous_state: State,
	state: State,
	current_index: usize,
}
impl<'a> Lexer<'a> {
	pub fn new(source: &str) -> Lexer {
		let alphebet = [
			'F', '_', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
			'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
		];
		let mut trie = Trie {
			root_children_ids: vec![],
			node_arena: vec![],
			alphebet,
		};
		for keyword in KEYWORDS.iter() {
			trie.insert_word(keyword);
			debug_assert_eq!(trie.is_match(keyword), Some(TrieMatch::ExactLongest));
		}
		Lexer {
			keywords_trie: trie,
			graphemes: UnicodeSegmentation::graphemes(source, true).collect(),
			previous_state: State::Start,
			state: State::Start,
			current_index: 0,
		}
	}
}
impl<'a> Iterator for Lexer<'a> {
	type Item = Lexeme;
	fn next(&mut self) -> Option<Self::Item> {
		if self.at_end() {
			return None;
		}

		self.state = State::Start;
		let mut lexeme_token = Token::new(self.current_index);
		self.handle_start(&mut lexeme_token);

		match self.state {
			State::Digit => self.handle_digit(&mut lexeme_token),
			State::Whitespace => self.handle_whitespace(&mut lexeme_token),
			State::Keyword => self.handle_keyword(&mut lexeme_token),
			State::Literal => self.handle_literal(&mut lexeme_token),
			_ => (),
		};
		debug_assert_eq!(self.state, State::Done);

		match self.previous_state {
			State::Start => None,
			State::Whitespace => Some(Lexeme::Whitespace(lexeme_token)),
			State::Keyword => {
				let keyword = Keyword::try_from(lexeme_token.source()).unwrap();
				Some(Lexeme::Keyword(lexeme_token, keyword))
			}
			State::Literal => Some(Lexeme::Literal(lexeme_token)),
			State::Digit => Some(Lexeme::Number(lexeme_token)),
			State::Comparison => {
				let cmp = Comparison::try_from(lexeme_token.source()).unwrap();
				Some(Lexeme::Comparison(lexeme_token, cmp))
			}
			State::Operator => Some(Lexeme::Operator(lexeme_token)),
			State::Punctuation => Some(Lexeme::Punctuation(lexeme_token)),
			State::Done => None,
		}
	}
}

impl<'a> Lexer<'a> {
	fn current(&self) -> Option<&str> {
		self.graphemes.get(self.current_index).cloned()
	}
	fn current_unchecked(&self) -> &str {
		self.graphemes[self.current_index]
	}

	#[allow(dead_code)]
	fn pervious(&self) -> Option<&str> {
		self.graphemes.get(self.current_index - 1).cloned()
	}
	fn previous_unchecked(&self) -> &str {
		self.graphemes[self.current_index - 1]
	}

	fn at_end(&self) -> bool {
		self.current_index >= self.graphemes.len()
	}
	fn add_one_to(&mut self, lexeme_token: &mut Token) {
		lexeme_token.push_str(self.current_unchecked());
		self.current_index += 1;
	}
	fn update_state(&mut self, new: State) {
		self.previous_state = self.state;
		self.state = new;
	}

	fn handle_start(&mut self, lexeme_token: &mut Token) {
		self.update_state({
			let c = self.current().unwrap();
			if Lexer::is_digit(c) {
				State::Digit
			} else if Lexer::is_whitespace(c) {
				State::Whitespace
			} else if Lexer::is_comparison_operator(c) {
				State::Comparison
			} else if Lexer::is_operator(c) {
				State::Operator
			} else if Lexer::is_punctuation(c) {
				State::Punctuation
			} else if self.keywords_trie.is_prefix(c) {
				State::Keyword
			} else {
				State::Literal
			}
		});

		self.add_one_to(lexeme_token);
		if self.at_end() {
			self.update_state(State::Done);
			return;
		}

		match self.state {
			State::Comparison => {
				let p = self.previous_unchecked();
				let c = self.current_unchecked();
				if (p == "<" || p == ">") && c == "=" {
					self.add_one_to(lexeme_token);
				}
				self.update_state(State::Done);
			}
			// matches **, !!, !p
			State::Operator => {
				let p = self.previous_unchecked();
				let c = self.current_unchecked();
				if (p == "*" && c == "*") || (p == "!" && (c == "!" || c == "p")) {
					self.add_one_to(lexeme_token);
				}
				self.update_state(State::Done);
			}
			State::Punctuation => {
				self.update_state(State::Done);
			}
			_ => (),
		}
	}

	fn handle_digit(&mut self, lexeme_token: &mut Token) {
		self.template_function_match_repeated(Lexer::is_digit, lexeme_token);
	}
	fn handle_whitespace(&mut self, lexeme_token: &mut Token) {
		self.template_function_match_repeated(Lexer::is_whitespace, lexeme_token);
	}
	fn handle_keyword(&mut self, lexeme_token: &mut Token) {
		let mut start = self.current_index;
		let mut found_exact = false;
		let mut last_prefix_len = 0;
		let mut matched_upto_id = None;
		while let Some(m) = self
			.keywords_trie
			.is_match_from(&lexeme_token.source()[last_prefix_len..], matched_upto_id)
		{
			match m {
				TrieMatch::Prefix(id) => {
					last_prefix_len = lexeme_token.source().len();
					matched_upto_id = Some(id)
				}
				TrieMatch::Exact(id) => {
					found_exact = true;
					start = self.current_index;
					last_prefix_len = lexeme_token.source().len();
					matched_upto_id = Some(id);
				}
				TrieMatch::ExactLongest => {
					self.update_state(State::Done);
					return;
				}
			}
			if self.at_end() {
				break;
			} else {
				self.add_one_to(lexeme_token);
			}
		}
		if found_exact {
			self.current_index = start;
			lexeme_token.truncate(last_prefix_len);
			self.update_state(State::Done);
		} else {
			self.update_state(State::Literal);
			self.handle_literal(lexeme_token);
		}
	}
	fn handle_literal(&mut self, lexeme_token: &mut Token) {
		let is_literal = |c: &str| -> bool {
			!Lexer::is_digit(c)
				&& !Lexer::is_whitespace(c)
				&& !Lexer::is_operator(c)
				&& !Lexer::is_punctuation(c)
				&& !Lexer::is_comparison_operator(c)
		};
		self.template_function_match_repeated(is_literal, lexeme_token);
	}

	// invariant: s contains only 1 grapheme
	fn is_whitespace(s: &str) -> bool {
		s.contains(char::is_whitespace)
	}

	// invariant: s contains only 1 grapheme
	fn is_digit(s: &str) -> bool {
		matches!(s, "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9")
	}

	fn is_comparison_operator(s: &str) -> bool {
		matches!(s, "<" | ">" | "=" | "<=" | ">=")
	}
	fn is_operator(s: &str) -> bool {
		matches!(s, "+" | "-" | "*" | "/" | "!" | "!!" | "!p" | "%" | "**" | "^")
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
	) {
		while let Some(c) = self.current() {
			if check(c) {
				self.add_one_to(lexeme_token);
			} else {
				break;
			}
		}
		self.update_state(State::Done);
	}
}
