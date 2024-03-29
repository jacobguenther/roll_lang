// File: src/interpreter/output/mod/.rs
// Author: Jacob Guenther(chmod777)
// License: AGPLv3

/*
Output struct generated by the interpreter and then formated into html or text.
*/

pub mod formats;

use super::InterpretError;
use crate::ast::number::*;

#[derive(Debug)]
pub struct Output {
	pub source: String,
	pub fragments: Vec<OutputFragment>,
	pub error: Option<InterpretError>,
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
	pub source: String,
	pub formula_fragments: FormulaFragments,
	pub result: Number,
}

pub type FormulaFragments = Vec<FormulaFragment>;
pub trait FormulaFragmentsT {
	fn push_str(&mut self, s: &str);
	fn push_new_str(&mut self, s: &str);
	fn push_number_roll(&mut self, roll: &NumberRoll);
	fn push_success_fail_roll(&mut self, roll: &SuccessFail);
	fn push_tooltip(&mut self, tooltip: &str);
}
impl FormulaFragmentsT for FormulaFragments {
	fn push_str(&mut self, s: &str) {
		match self.last_mut() {
			Some(FormulaFragment::Basic(string, None)) => {
				string.push_str(s);
			}
			_ => self.push(FormulaFragment::Basic(String::from(s), None)),
		}
	}
	fn push_new_str(&mut self, s: &str) {
		self.push(FormulaFragment::Basic(s.to_owned(), None));
	}
	fn push_number_roll(&mut self, roll: &NumberRoll) {
		if let Some(FormulaFragment::NumberRolls(_, rolls, _)) = self.last_mut() {
			rolls.push(*roll);
		} else {
			self.push(FormulaFragment::NumberRolls(*roll, Vec::new(), None));
		}
	}
	fn push_success_fail_roll(&mut self, success_fail: &SuccessFail) {
		if let Some(FormulaFragment::SuccessFailRolls(_, rolls, _)) = self.last_mut() {
			rolls.push(*success_fail);
		} else {
			self.push(FormulaFragment::SuccessFailRolls(
				*success_fail,
				Vec::new(),
				None,
			));
		}
	}
	fn push_tooltip(&mut self, tooltip: &str) {
		match self.last_mut() {
			Some(FormulaFragment::NumberRolls(_, _, tip))
			| Some(FormulaFragment::SuccessFailRolls(_, _, tip))
			| Some(FormulaFragment::Basic(_, tip)) => {
				*tip = Some(tooltip.to_owned());
			}
			_ => panic!("Unexpected push_tooltip"),
		}
		self.push(FormulaFragment::Basic(String::new(), None));
	}
}
#[derive(Debug, Clone)]
pub enum FormulaFragment {
	// string, tooltip
	Basic(String, Option<String>),
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
			if let NumberRoll::Counted(int, _) = number_roll {
				res += int;
			}
		}
		res
	}
}
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct NumberRollValueModifier {
	pub amount: Integer,
}
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ReasonNotCounted {
	Dropped,
	Rerolled,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum NumberRoll {
	Counted(Integer, Option<NumberRollValueModifier>),
	NotCounted(Integer, ReasonNotCounted),
}
impl NumberRoll {
	pub fn roll_value(&self) -> Integer {
		match self {
			NumberRoll::Counted(i, _) | NumberRoll::NotCounted(i, _) => *i,
		}
	}
	pub fn value(&self) -> Integer {
		match self {
			NumberRoll::Counted(i, Some(m)) => i + m.amount,
			NumberRoll::Counted(i, _) | NumberRoll::NotCounted(i, _) => *i,
		}
	}
}
pub type SuccessFailRolls = Vec<SuccessFail>;

#[derive(Debug, Copy, Clone)]
pub enum SuccessFail {
	Success(Integer),
	Fail(Integer),
	CriticalSuccess(Integer),
	CriticalFail(Integer),
}
