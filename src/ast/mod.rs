// File: ast.rs

pub mod number;

use number::*;
use super::parser::ParseError;

pub type Root = Vec<Node>;

#[derive(Debug, Clone)]
pub enum Node {
	StringLiteral(StringLiteral),
	Roll(Roll),
	ParseError(ParseError),
}

#[derive(Debug, Clone)]
pub struct StringLiteral {
	s: String,
	previous_size: usize,
}
impl Default for StringLiteral {
	fn default() -> StringLiteral {
		StringLiteral {
			s: String::new(),
			previous_size: 0,
		}
	}
}
impl StringLiteral {
	pub fn new(s: &str) -> StringLiteral {
		StringLiteral {
			s: s.to_owned(),
			previous_size: 0,
		}
	}
	pub fn str(&self) -> &str {
		&self.s
	}
	pub fn append(&mut self, s: &str) {
		self.previous_size = self.s.len();
		self.s.push_str(s);
	}
}

#[derive(Debug, Clone)]
pub enum Roll {
	ExplicitRoll(Expression),
	InlineRoll(Expression),
}
#[derive(Debug, Clone)]
pub enum Expression {
	Add(Box<Expression>, MulDiv),
	Subtract(Box<Expression>, MulDiv),
	MulDiv(MulDiv),
}

#[derive(Debug, Clone)]
pub enum MulDiv {
	Multiply(Box<MulDiv>, Power),
	Divide(Box<MulDiv>, Power),
	Power(Power),
}
#[derive(Debug, Clone)]
pub enum Power {
	Pow(Unary, Box<Power>),
	Unary(Unary),
}
#[derive(Debug, Clone)]
pub enum Unary {
	Minus(Option<InlineComment>, Box<Unary>),
	Atom(Atom),
}
#[derive(Debug, Clone)]
pub enum Atom {
	Number(Option<InlineComment>, Number, Option<InlineComment>),
	Dice(Option<InlineComment>, Dice, Option<InlineComment>),
	Function(Option<InlineComment>, Function, Option<InlineComment>),
	RollQuery(Option<InlineComment>, RollQuery, Option<InlineComment>),
	Expression(Option<InlineComment>, Box<Expression>, Option<InlineComment>),
}


#[derive(Debug, Clone)]
pub struct InlineComment {
	comment: String,
}
impl InlineComment {
	pub fn new() -> InlineComment {
		InlineComment {
			comment: String::new(),
		}
	}
	pub fn append(&mut self, s: &str) {
		self.comment.push_str(s);
	}
	pub fn comment(&self) -> &str {
		&self.comment
	}
}

#[derive(Debug, Clone)]
pub enum Dice {
	Normal(Normal, Vec<Modifier>, Option<InlineComment>),
	Fate(Fate, Vec<Modifier>, Option<InlineComment>),
	Computed(Computed, Vec<Modifier>, Option<InlineComment>),
}

#[derive(Debug, Copy, Clone)]
pub struct Normal {
	pub count: Integer,
	pub sides: Integer,
}
#[derive(Debug, Copy, Clone)]
pub struct Fate {
	pub count: Integer,
}
#[derive(Debug, Clone)]
pub struct Computed {
	pub count: Box<Expression>,
	pub sides: Box<Expression>,
}

#[derive(Debug, Copy, Clone)]
pub enum Modifier {
	Exploding(Comparison, Option<Integer>),
	Compounding(Comparison, Option<Integer>),
	Reroll(Comparison, Integer),
	KeepHighest(Integer),
	KeepLowest(Integer),
	DropHighest(Integer),
	DropLowest(Integer),
	CriticalSuccess(Comparison, Integer),
	CriticalFailure(Comparison, Integer),
	Success(Comparison, Integer),
}

#[derive(Debug, Copy, Clone)]
pub enum Comparison {
	LessThan,
	GreaterThan,
	LessThanEqual,
	GreaterThanEqual,
	Equal,
}

#[derive(Debug, Clone)]
pub enum Function {
	Floor(Box<Expression>),
	Ceil(Box<Expression>),
	Round(Box<Expression>),
	Abs(Box<Expression>),
}

#[derive(Debug, Clone)]
pub struct RollQuery {
	pub prompt: String,
	pub default: Option<Box<Expression>>,
}
impl RollQuery {
	pub fn new() -> RollQuery {
		RollQuery {
			prompt: String::new(),
			default: None,
		}
	}
	pub fn append_prompt(&mut self, s: &str) {
		self.prompt.push_str(s);
	}
}
