// File: parser/error.rs

use crate::lexer::{keywords::Keyword, token::Token};

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
pub enum ParseError {
	UnexpectedToken(Token),
	UnexpectedTooltip(String),

	Expected(Expecting),
	ExpectedOneOf(Vec<Expecting>),

	MultipleTypesOfExpandingModifiersNotSupported,
	MultipleDropKeepModifiersNotSupported,
	MultipleSortModifiersNotSupported,

	DoesNotMatch,

	OutOfBounds,
	Unknown,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
pub enum Expecting {
	Punctuation(String),
	Literal(String),
	Keyword(Keyword),
	OneOfKeywords(Vec<Keyword>),
	Operator(String),

	Dice,
	Number,
	Integer,
	Float,
	Function,
	Comparison,
	InlineRoll,
	ParenthesesExpression,

	RollQuery,
	Macro,
}
