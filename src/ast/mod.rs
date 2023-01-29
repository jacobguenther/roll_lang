// File: ast.rs

pub mod error;
pub mod number;

use crate::parser::error::ParseError;
use number::*;

pub type Root = Vec<Node>;

#[derive(Debug, Clone)]
pub enum Node {
	StringLiteral(String),
	Macro(Macro),
	Roll(Box<Roll>),
	ParseError(ParseError),
}
#[derive(Debug, Clone)]
pub struct Macro {
	pub start: usize,
	pub end: usize,
	pub name: String,
}
#[derive(Debug, Clone)]
pub enum Roll {
	ExplicitRoll(Expression, String),
	InlineRoll(Expression, String),
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

type Tooltip = Option<String>;
type Comment = Option<String>;

#[derive(Debug, Clone)]
pub enum Unary {
	Minus(Comment, Box<Unary>),
	Atom(Comment, Atom, Comment, Option<Vec<Expression>>),
}

#[derive(Debug, Clone)]
pub enum Atom {
	Number(Number, Tooltip),
	Dice(Dice, Tooltip),
	Function(Function),
	RollQuery(RollQuery),
	ParenthesesExpression(Box<Expression>),
	InlineRoll(Box<Expression>),
	Macro(Macro),
}

#[derive(Debug, Clone)]
pub enum Dice {
	Normal(Normal, Modifiers),
	Fate(Fate, Modifiers),
	Computed(Computed, Modifiers),
}
impl Dice {
	pub fn default_count() -> Integer {
		1
	}
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

#[derive(Debug, Clone, Default)]
pub struct Modifiers {
	pub expanding: Option<Expanding>,
	pub reroll: Vec<Reroll>,
	pub reroll_once: Option<Reroll>,
	pub drop_keep: Option<DropKeep>,
	pub successes: Vec<Successes>,
	pub sort: Option<Sort>,
}

#[derive(Debug, Clone)]
pub enum Expanding {
	Exploding(Vec<Reroll>),
	Penetrating(Vec<Reroll>),
	Compounding(Vec<Reroll>),
}
#[derive(Debug, Copy, Clone)]
pub struct Reroll {
	pub comparison: Comparison,
	pub comparison_point: Option<Integer>,
}
pub trait RerollT {
	fn new(comparison: Comparison, comparison_point: Option<Integer>) -> Self;
}
impl RerollT for Reroll {
	fn new(comparison: Comparison, comparison_point: Option<Integer>) -> Reroll {
		Reroll {
			comparison,
			comparison_point,
		}
	}
}

#[derive(Debug, Copy, Clone)]
pub enum Successes {
	CriticalSuccess(Comparison, Integer),
	CriticalFailure(Comparison, Integer),
	Success(Comparison, Integer),
	Failure(Comparison, Integer),
}

#[derive(Debug, Copy, Clone)]
pub enum DropKeep {
	DropLowest(Integer),
	DropHighest(Integer),
	KeepLowest(Integer),
	KeepHighest(Integer),
}

#[derive(Debug, Copy, Clone)]
pub enum Sort {
	Ascending,
	Descending,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Comparison {
	LessThan,
	GreaterThan,
	LessThanEqual,
	GreaterThanEqual,
	Equal,
}
use std::convert::TryFrom;
impl TryFrom<&str> for Comparison {
	type Error = ();
	fn try_from(s: &str) -> Result<Comparison, Self::Error> {
		match s {
			"<" => Ok(Comparison::LessThan),
			">" => Ok(Comparison::GreaterThan),
			"=" => Ok(Comparison::Equal),
			"<=" => Ok(Comparison::LessThanEqual),
			">=" => Ok(Comparison::GreaterThanEqual),
			_ => Err(()),
		}
	}
}

#[derive(Debug, Clone)]
pub enum Function {
	Floor(Box<Expression>),
	Ceil(Box<Expression>),
	Round(Box<Expression>),
	RoundHalfDown(Box<Expression>),
	Abs(Box<Expression>),
}

#[derive(Debug, Clone)]
pub struct RollQuery {
	pub prompt: String,
	pub default: String,
}
impl Default for RollQuery {
	fn default() -> RollQuery {
		Self {
			prompt: String::new(),
			default: String::new(),
		}
	}
}
impl RollQuery {
	pub fn new(prompt: &str, default: &str) -> RollQuery {
		Self {
			prompt: prompt.to_owned(),
			default: default.to_owned(),
		}
	}
	pub fn as_expression(&self) -> Expression {
		Expression::MulDiv(MulDiv::Power(Power::Unary(Unary::Atom(
			None,
			Atom::RollQuery(RollQuery::new(&self.prompt, &self.default)),
			None,
			None,
		))))
	}
}
