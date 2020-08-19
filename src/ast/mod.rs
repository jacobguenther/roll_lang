// File: ast.rs

pub mod number;

use number::*;
use super::parser::ParseError;

pub type Root = Vec<Node>;

#[derive(Debug, Clone)]
pub enum Node {
	StringLiteral(String),
	Roll(Roll),
	Macro(Macro),
	ParseError(ParseError),
}
#[derive(Debug, Clone)]
pub struct Macro {
	pub name: String,
}
// #[derive(Debug, Clone)]
// pub struct ExpressionMacro {
// 	pub name: String,
// 	pub expression: Box<Expression>,
// }
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
	Minus(Option<String>, Box<Unary>),
	Atom(Option<String>, Atom, Option<String>),
}
#[derive(Debug, Clone)]
pub enum Atom {
	Number(Number),
	Dice(Dice),
	Function(Function),
	RollQuery(RollQuery),
	ParenthesesExpression(Box<Expression>),
}

#[derive(Debug, Clone)]
pub enum Dice {
	Normal(Normal, Modifiers, Option<String>),
	Fate(Fate, Modifiers, Option<String>),
	Computed(Computed, Modifiers, Option<String>),
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

#[derive(Debug, Clone)]
pub struct Modifiers {
	pub expanding: Option<Expanding>,
	pub reroll_modifiers: Vec<Reroll>,
	pub post_modifiers: Vec<PostModifier>,
}
impl Modifiers {
	pub fn new() -> Self {
		Modifiers {
			expanding: None,
			reroll_modifiers: Vec::new(),
			post_modifiers: Vec::new(),
		}
	}
}

#[derive(Debug, Copy, Clone)]
pub enum Modifier {
	Reroll(Reroll),
	Expanding(Expanding),
	PostModifier(PostModifier),
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
	fn new(comparison: Comparison, comparison_point: Option<Integer>) -> Self {
		Self {
			comparison: comparison,
			comparison_point: comparison_point,
		}
	}
}
#[derive(Debug, Copy, Clone)]
pub enum Expanding {
	Exploding(Exploding),
	Compounding(Compounding),
	Penetrating(Penetrating),
}
pub type Exploding = Reroll;
pub type Compounding = Reroll;
pub type Penetrating = Reroll;

#[derive(Debug, Copy, Clone)]
pub enum PostModifier {
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
	pub default: String,
}
impl RollQuery {
	pub fn new() -> RollQuery {
		RollQuery {
			prompt: String::new(),
			default: String::new(),
		}
	}
	pub fn as_expression(&self) -> Expression {
		Expression::MulDiv(
			MulDiv::Power(
				Power::Unary(
					Unary::Atom(
						None,
						Atom::RollQuery(
							RollQuery {
								prompt: self.prompt.clone(),
								default: self.default.clone(),
							}
						),
						None
					)
				)
			)
		)
	}
}
