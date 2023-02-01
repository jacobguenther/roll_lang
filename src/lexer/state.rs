// File: lexer/state.rs

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum State {
	Start,
	Whitespace,
	Literal,
	Keyword,
	Punctuation,
	Comparison,
	Operator,
	Integer,
	Float,
	Done,
}
