// File: lexer/mod.rs

pub mod lexeme;
mod state;
pub mod token;
pub mod keywords;

use lexeme::{Lexeme, LexemeType};
use state::State;
use token::{Token, TokenT};

use keywords::KEYWORDS;

use unicode_segmentation::UnicodeSegmentation;

pub struct Lexer<'a> {
	graphemes: Vec<&'a str>,
	state: State,
	current_index: usize,
}
impl<'a> Lexer<'a> {
	pub fn new(source: &str) -> Lexer {
		Lexer {
			graphemes: UnicodeSegmentation::graphemes(source, true).collect(),
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
		let mut lexeme = Lexeme::Literal(Token::new(self.current_index));

		loop {
			match self.state {
				State::Start => self.handle_start(&mut lexeme),
				State::Whitespace => self.handle_whitespace(&mut lexeme),
				State::Keyword => self.handle_keyword(&mut lexeme),
				State::Literal => self.handle_literal(&mut lexeme),
				State::Digit => self.handle_digit(&mut lexeme),
				State::Done => break,
			};
		}

		Some(lexeme)
	}
}

trait LexerPrivateT {
	fn current_char(&self) -> Option<String>;
	fn at_end(&self) -> bool;

	fn add_one(&mut self, lexeme: &mut Lexeme);
	fn remove_one(&mut self, lexeme: &mut Lexeme);

	fn template_function_match_repeated(&mut self, check: fn(&str) -> bool, lexeme: &mut Lexeme);

	fn handle_start(&mut self, lexeme: &mut Lexeme);
	fn handle_whitespace(&mut self, lexeme: &mut Lexeme);
	fn handle_keyword(&mut self, lexeme: &mut Lexeme);
	fn handle_literal(&mut self, lexeme: &mut Lexeme);
	fn handle_digit(&mut self, lexeme: &mut Lexeme);

	fn is_keyword_substr(s: &str) -> bool;
	fn is_keyword(s: &str) -> bool;
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

	fn add_one(&mut self, lexeme: &mut Lexeme) {
		lexeme.push_str(&self.current_char().unwrap());
		self.current_index += 1;
	}
	fn remove_one(&mut self, lexeme: &mut Lexeme) {
		let source = lexeme.source();
		let mut index = source.len() - 1;
		while !source.is_char_boundary(index) {
			index -= 1;
		}
		lexeme.truncate(index);
		self.current_index -= 1;
	}

	fn template_function_match_repeated(&mut self, check: fn(&str) -> bool, lexeme: &mut Lexeme) {
		loop {
			let c = self.current_char().unwrap();
			match check(&c) {
				true => {
					self.add_one(lexeme);
					if self.at_end() {
						self.state = State::Done;
						break;
					}
				}
				false => {
					self.state = State::Done;
					break;
				}
			}
		}
	}

	fn handle_start(&mut self, lexeme: &mut Lexeme) {
		let c = self.current_char().unwrap();

		if Lexer::is_digit(&c) {
			self.add_one(lexeme);
			*lexeme = lexeme.into(&LexemeType::Number);
			if self.at_end() {
				self.state = State::Done;
			} else {
				self.state = State::Digit;
			}
		} else if Lexer::is_whitespace(&c) {
			self.add_one(lexeme);
			*lexeme = lexeme.into(&LexemeType::Whitespace);
			if self.at_end() {
				self.state = State::Done;
			} else {
				self.state = State::Whitespace;
			}
		} else if Lexer::is_comparison_operator(&c) {
			self.add_one(lexeme);
			*lexeme = lexeme.into(&LexemeType::Comparison);
			if !self.at_end() && ((c == "<" || c == ">") && self.current_char().unwrap() == "=") {
				self.add_one(lexeme);
			}
			self.state = State::Done;
		} else if Lexer::is_operator(&c) {
			self.add_one(lexeme);
			*lexeme = lexeme.into(&LexemeType::Operator);
			if !self.at_end()
				&& ((c == "*" && self.current_char().unwrap() == "*")
					|| (c == "!" && self.current_char().unwrap() == "!"
						|| self.current_char().unwrap() == "p"))
			{
				self.add_one(lexeme);
			}
			self.state = State::Done;
		} else if Lexer::is_punctuation(&c) {
			self.add_one(lexeme);
			*lexeme = lexeme.into(&LexemeType::Punctuation);
			self.state = State::Done;
		} else if Lexer::is_keyword_substr(lexeme.source()) {
			*lexeme = lexeme.into(&LexemeType::Keyword);
			self.state = State::Keyword;
		} else {
			*lexeme = lexeme.into(&LexemeType::Literal);
			self.state = State::Literal;
		}
	}
	fn handle_whitespace(&mut self, lexeme: &mut Lexeme) {
		self.template_function_match_repeated(Lexer::is_whitespace, lexeme);
	}
	fn handle_literal(&mut self, lexeme: &mut Lexeme) {
		loop {
			if self.at_end() {
				self.state = State::Done;
				break;
			}
			let c = self.current_char().unwrap();
			if !Lexer::is_digit(&c)
				&& !Lexer::is_whitespace(&c)
				&& !Lexer::is_operator(&c)
				&& !Lexer::is_punctuation(&c)
				&& !Lexer::is_comparison_operator(&c)
			{
				self.add_one(lexeme);
			} else {
				self.state = State::Done;
				break;
			}
		}
	}
	fn handle_keyword(&mut self, lexeme: &mut Lexeme) {
		let start = self.current_index;
		while Lexer::is_keyword_substr(lexeme.source()) && !self.at_end() {
			println!("{:?}", lexeme);
			self.add_one(lexeme);
		}
		println!("{:?}", lexeme);
		while !Lexer::is_keyword(lexeme.source()) && self.current_index > start {
			self.remove_one(lexeme);
		}
		if Lexer::is_keyword(lexeme.source()) {
			self.state = State::Done;
		} else {
			self.state = State::Literal;
			*lexeme = lexeme.into(&LexemeType::Literal);
			self.handle_literal(lexeme);
		}
	}
	fn handle_digit(&mut self, lexeme: &mut Lexeme) {
		self.template_function_match_repeated(Lexer::is_digit, lexeme);
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
	fn is_keyword(s: &str) -> bool {
		for keyword in KEYWORDS.iter() {
			if **keyword == *s {
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
