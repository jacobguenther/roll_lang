// File: interpreter/output.rs

use super::InterpretError;
use crate::ast::number::*;

#[derive(Debug)]
pub struct Output {
	pub(super) source: String,
	pub(super) fragments: Vec<OutputFragment>,
	pub(super) error: Option<InterpretError>,
}
impl Output {
	pub(super) fn new(source: &str) -> Output {
		Output {
			source: source.to_owned(),
			fragments: Vec::new(),
			error: None,
		}
	}
}

#[derive(Debug, Clone)]
pub enum OutputFragment {
	StringLit(String),
	Roll(RollType),
}
#[derive(Debug, Clone)]
pub enum RollType {
	ExplicitRoll(ExpressionOutput),
	InlineRoll(ExpressionOutput),
}
#[derive(Debug, Clone)]
pub struct ExpressionOutput {
	pub(super) formula_fragments: FormulaFragments,
	pub(super) result: Number,
}

pub(super) type FormulaFragments = Vec<FormulaFragment>;
pub(super) trait FormulaFragmentsT {
	fn push_str(&mut self, s: &str);
	fn push_number_roll(&mut self, roll: &NumberRoll);
	fn push_success_fail_roll(&mut self, roll: &SuccessFail);
	fn push_tooltip(&mut self, tooltip: &str);
}
impl FormulaFragmentsT for FormulaFragments {
	fn push_str(&mut self, s: &str) {
		match self.last_mut() {
			Some(FormulaFragment::Basic(string)) => {
				string.push_str(s);
			},
			_ => self.push(FormulaFragment::Basic(String::from(s))),
		}
	}
	fn push_number_roll(&mut self, roll: &NumberRoll) {
		match self.last_mut() {
			Some(FormulaFragment::NumberRolls(_first, rolls, _tooltip)) => rolls.push(*roll),
			_ => self.push(FormulaFragment::NumberRolls(*roll, vec!(), None)),
		}
	}
	fn push_success_fail_roll(&mut self, success_fail: &SuccessFail) {
		match self.last_mut() {
			Some(FormulaFragment::SuccessFailRolls(_first, rolls, _tooltip)) => rolls.push(*success_fail),
			_ => self.push(FormulaFragment::SuccessFailRolls(*success_fail, vec!(), None)),
		}
	}
	fn push_tooltip(&mut self, tooltip: &str) {
		match self.last_mut() {
			Some(FormulaFragment::NumberRolls(_, _, tip))
			| Some(FormulaFragment::SuccessFailRolls(_, _, tip))
				=> *tip = Some(tooltip.to_owned()),
			_ => (),
		}
	}
}
#[derive(Debug, Clone)]
pub enum FormulaFragment {
	Basic(String),
	// first roll, rest of rolls, tooltip
	NumberRolls(NumberRoll, NumberRolls, Option<String>),
	SuccessFailRolls(SuccessFail, SuccessFailRolls, Option<String>),
}

pub type NumberRolls = Vec<NumberRoll>;
pub trait NumberRollsT {
	fn sum_counted_rolls(&self) -> Integer;
}
impl NumberRollsT for NumberRolls {
	fn sum_counted_rolls(&self) -> Integer {
		let mut res = 0;
		for number_roll in self {
			match number_roll {
				NumberRoll::Counted(int) => res += int.value(),
				_ => (),
			}
		}
		Integer::new(res)
	}
}
#[derive(Debug, Copy, Clone)]
pub enum NumberRoll {
	Counted(Integer),
	NotCounted(Integer),
}


pub type SuccessFailRolls = Vec<SuccessFail>;

#[derive(Debug, Copy, Clone)]
pub enum SuccessFail {
	Success(Integer),
	Fail(Integer),
	CriticalSuccess(Integer),
	CriticalFail(Integer),
}