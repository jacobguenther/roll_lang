// File: lexer/mod.rs

pub mod keywords;
pub mod lexeme;
mod state;
pub mod token;

use lexeme::Lexeme;
use state::State;
use token::{Token, TokenT};

use std::convert::TryFrom;

use keywords::{Keyword, KEYWORDS};

use unicode_segmentation::UnicodeSegmentation;

pub struct Lexer<'a> {
	graphemes: Vec<&'a str>,
	previous_state: State,
	state: State,
	current_index: usize,
}
impl<'a> Lexer<'a> {
	pub fn new(source: &str) -> Lexer {
		Lexer {
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

		loop {
			match self.state {
				State::Start => self.handle_start(&mut lexeme_token),
				State::Whitespace => self.handle_whitespace(&mut lexeme_token),
				State::Keyword => self.handle_keyword(&mut lexeme_token),
				State::Literal => self.handle_literal(&mut lexeme_token),
				State::Digit => self.handle_digit(&mut lexeme_token),
				State::Comparison | State::Operator | State::Punctuation => break,
				State::Done => break,
			};
		}

		match self.previous_state {
			State::Start => None,
			State::Whitespace => Some(Lexeme::Whitespace(lexeme_token)),
			State::Keyword => Some(Lexeme::Keyword(
				lexeme_token.clone(),
				Keyword::try_from(lexeme_token.source()).unwrap(),
			)),
			State::Literal => Some(Lexeme::Literal(lexeme_token)),
			State::Digit => Some(Lexeme::Number(lexeme_token)),
			State::Comparison => Some(Lexeme::Comparison(lexeme_token)),
			State::Operator => Some(Lexeme::Operator(lexeme_token)),
			State::Punctuation => Some(Lexeme::Punctuation(lexeme_token)),
			State::Done => None,
		}
	}
}

trait LexerPrivateT {
	fn current_char(&self) -> Option<String>;
	fn at_end(&self) -> bool;

	fn update_state(&mut self, new: State);

	fn add_one(&mut self, lexeme_token: &mut Token);
	fn remove_one(&mut self, lexeme_token: &mut Token);

	fn template_function_match_repeated(
		&mut self,
		check: fn(&str) -> bool,
		lexeme_token: &mut Token,
	);

	fn handle_start(&mut self, lexeme_token: &mut Token);
	fn handle_whitespace(&mut self, lexeme_token: &mut Token);
	fn handle_keyword(&mut self, lexeme_token: &mut Token);
	fn handle_literal(&mut self, lexeme_token: &mut Token);
	fn handle_digit(&mut self, lexeme_token: &mut Token);

	fn is_keyword_substr(s: &str) -> bool;
	fn is_whitespace(s: &str) -> bool;
	fn is_digit(s: &str) -> bool;
	fn is_comparison_operator(s: &str) -> bool;
	fn is_operator(s: &str) -> bool;
	fn is_punctuation(s: &str) -> bool;
}
impl<'a> LexerPrivateT for Lexer<'a> {
	fn current_char(&self) -> Option<String> {
		if self.current_index < self.graphemes.len() {
			Some(self.graphemes[self.current_index].to_owned())
		} else {
			None
		}
	}
	fn at_end(&self) -> bool {
		self.current_index >= self.graphemes.len()
	}

	fn update_state(&mut self, new: State) {
		self.previous_state = self.state;
		self.state = new;
	}

	fn add_one(&mut self, lexeme: &mut Token) {
		lexeme.push_str(&self.current_char().unwrap());
		self.current_index += 1;
	}
	fn remove_one(&mut self, lexeme: &mut Token) {
		let source = lexeme.source();
		let mut index = source.len() - 1;
		while !source.is_char_boundary(index) {
			index -= 1;
		}
		lexeme.truncate(index);
		self.current_index -= 1;
	}

	fn template_function_match_repeated(&mut self, check: fn(&str) -> bool, lexeme: &mut Token) {
		loop {
			let c = self.current_char().unwrap();
			match check(&c) {
				true => {
					self.add_one(lexeme);
					if self.at_end() {
						self.update_state(State::Done);
						break;
					}
				}
				false => {
					self.update_state(State::Done);
					break;
				}
			}
		}
	}

	fn handle_start(&mut self, lexeme_token: &mut Token) {
		let c = self.current_char().unwrap();

		if Lexer::is_digit(&c) {
			self.update_state(State::Digit);
			self.add_one(lexeme_token);
			if self.at_end() {
				self.update_state(State::Done);
			}
		} else if Lexer::is_whitespace(&c) {
			self.update_state(State::Whitespace);
			self.add_one(lexeme_token);
			if self.at_end() {
				self.update_state(State::Done);
			}
		} else if Lexer::is_comparison_operator(&c) {
			self.update_state(State::Comparison);
			self.add_one(lexeme_token);
			if !self.at_end() && ((c == "<" || c == ">") && self.current_char().unwrap() == "=") {
				self.add_one(lexeme_token);
			}
			self.update_state(State::Done);
		} else if Lexer::is_operator(&c) {
			self.update_state(State::Operator);
			self.add_one(lexeme_token);
			if !self.at_end()
				&& ((c == "*" && self.current_char().unwrap() == "*")
					|| (c == "!" && self.current_char().unwrap() == "!"
						|| self.current_char().unwrap() == "p"))
			{
				self.add_one(lexeme_token);
			}
			self.update_state(State::Done);
		} else if Lexer::is_punctuation(&c) {
			self.update_state(State::Punctuation);
			self.add_one(lexeme_token);
			self.update_state(State::Done);
		} else if Lexer::is_keyword_substr(lexeme_token.source()) {
			self.update_state(State::Keyword);
		} else {
			self.update_state(State::Literal);
		}
	}
	fn handle_whitespace(&mut self, lexeme_token: &mut Token) {
		self.template_function_match_repeated(Lexer::is_whitespace, lexeme_token);
	}
	fn handle_literal(&mut self, lexeme_token: &mut Token) {
		loop {
			if self.at_end() {
				self.update_state(State::Done);
				break;
			}
			let c = self.current_char().unwrap();
			if !Lexer::is_digit(&c)
				&& !Lexer::is_whitespace(&c)
				&& !Lexer::is_operator(&c)
				&& !Lexer::is_punctuation(&c)
				&& !Lexer::is_comparison_operator(&c)
			{
				self.add_one(lexeme_token);
			} else {
				self.update_state(State::Done);
				break;
			}
		}
	}
	fn handle_keyword(&mut self, lexeme_token: &mut Token) {
		let start = self.current_index;
		while Lexer::is_keyword_substr(lexeme_token.source()) && !self.at_end() {
			self.add_one(lexeme_token);
		}
		while keywords::Keyword::try_from(lexeme_token.source()).is_err()
			&& self.current_index > start
		{
			self.remove_one(lexeme_token);
		}
		if keywords::Keyword::try_from(lexeme_token.source()).is_ok() {
			self.update_state(State::Done);
		} else {
			self.update_state(State::Literal);
			self.handle_literal(lexeme_token);
		}
	}
	fn handle_digit(&mut self, lexeme_token: &mut Token) {
		self.template_function_match_repeated(Lexer::is_digit, lexeme_token);
	}

	fn is_whitespace(s: &str) -> bool {
		for c in s.chars() {
			if !c.is_whitespace() {
				return false;
			}
		}
		true
	}
	fn is_digit(s: &str) -> bool {
		for c in s.chars() {
			if !c.is_ascii_digit() {
				return false;
			}
		}
		true
	}

	fn is_keyword_substr(s: &str) -> bool {
		for keyword in KEYWORDS.iter() {
			if let Some(_substr_byte_index) = keyword.find(s) {
				return true;
			}
		}
		false
	}
	fn is_comparison_operator(s: &str) -> bool {
		matches!(s, "<" | ">" | "=" | "<=" | ">=")
	}
	fn is_operator(s: &str) -> bool {
		matches!(s, "+" | "-" | "*" | "/" | "!" | "!!" | "%" | "**" | "^")
	}
	fn is_punctuation(s: &str) -> bool {
		matches!(
			s,
			"[" | "]" | "(" | ")" | "{" | "}" | "\\" | "." | "?" | "|" | "#"
		)
	}
}
