// File: interpreter/error.rs

#[cfg(feature = "serialize")]
use serde::{
	Deserialize,
	Serialize,
};

use crate::ast::error::OperatorError;
use crate::parser::error::ParseError;

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
pub enum NotSupportedYet {
	FateDice,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
pub enum InterpretError {
	LexError,
	ParseError(ParseError),
	OperatorError(OperatorError),

	DiceWithFewerThanOneSides,
	DiceCountMustBeAnInteger,
	DiceSidesMustBeAnInteger,

	InfiniteRerollsDetected,

	FailedGettingInputFromPrompt(String),

	InterpreterConstructedWithoutMacros,
	NoMacroNamed(String),
	ErrorInMacro(String, Box<InterpretError>),
	ThisMacroCannotBeNested(String),

	MultipleTypesOfExpandingModifiersNotSupported,

	NotSupportedYet(NotSupportedYet),
	UnknownInterpreterError,
}

impl From<ParseError> for InterpretError {
	fn from(parse_error: ParseError) -> Self {
		Self::ParseError(parse_error)
	}
}

impl From<OperatorError> for InterpretError {
	fn from(operator_error: OperatorError) -> Self {
		Self::OperatorError(operator_error)
	}
}
