// File: parser/error.rs

use crate::lexer::{keywords::Keyword, token::Token};

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
pub enum ParseError {
	UnexpectedToken(Token),

	ExpectedPunctuation(String),
	ExpectedLiteral(String),
	ExpectedKeyword(Keyword),
	ExpectedOneOfKeywords(Vec<Keyword>),
	ExpectedOperator(String),

	MultipleTypesOfExpandingModifiersNotSupported,
	MultipleDropKeepModifiersNotSupported,
	MultipleSortModifiersNotSupported,

	ExpectedInteger,
	DoesNotMatch,

	OutOfBounds,
	UnexpectedTooltip(String),
	Unknown,
}
