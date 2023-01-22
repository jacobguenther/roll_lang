// File: src/tests/lexer/mod.rs

use crate::lexer::token::*;
use crate::lexer::Lexer;

use std::iter::Iterator;

#[test]
fn comparisons() {
	let comparison_helper = |source| {
		let lexemes = Lexer::new(source).collect::<Vec<_>>();
		let mut lexemes_iter = lexemes.iter();
		let lexeme = lexemes_iter.next().unwrap();
		assert!(lexeme.is_comparison());
		assert_eq!(source, lexeme.token().source());
		assert_eq!(None, lexemes_iter.next());
	};
	comparison_helper("=");
	comparison_helper("<");
	comparison_helper(">");
	comparison_helper("<=");
	comparison_helper(">=");
}

#[test]
fn keywords() {
	let keyword_helper = |source| {
		let lexemes = Lexer::new(source).collect::<Vec<_>>();
		let mut lexemes_iter = lexemes.iter();
		let lexeme = lexemes_iter.next().unwrap();
		assert!(lexeme.is_keyword());
		assert_eq!(source, lexeme.token().source());
		assert_eq!(None, lexemes_iter.next());
	};
	keyword_helper("r");
	keyword_helper("ro");
	keyword_helper("roll");
	keyword_helper("reroll");
	keyword_helper("round");
	keyword_helper("round_half_down");
}
