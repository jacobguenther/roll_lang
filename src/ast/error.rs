// File: ast/error.rs

#[derive(Debug, Copy, Clone)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
pub enum OperatorError {
	DivideByZero,
}
