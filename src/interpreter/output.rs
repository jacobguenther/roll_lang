// File: interpreter/output.rs

use super::InterpretError;
use crate::ast::number::*;

#[cfg(feature = "serialize")]
use serde::{Deserialize, Serialize};

#[derive(Debug)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
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
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
pub enum OutputFragment {
	StringLit(String),
	Roll(RollType),
}
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
pub enum RollType {
	ExplicitRoll(ExpressionOutput),
	InlineRoll(ExpressionOutput),
}
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
pub struct ExpressionOutput {
	pub formula_fragments: FormulaFragments,
	pub result: Number,
}

pub type FormulaFragments = Vec<FormulaFragment>;
pub trait FormulaFragmentsT {
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
			}
			_ => self.push(FormulaFragment::Basic(String::from(s))),
		}
	}
	fn push_number_roll(&mut self, roll: &NumberRoll) {
		match self.last_mut() {
			Some(FormulaFragment::NumberRolls(_first, rolls, _tooltip)) => rolls.push(*roll),
			_ => self.push(FormulaFragment::NumberRolls(*roll, vec![], None)),
		}
	}
	fn push_success_fail_roll(&mut self, success_fail: &SuccessFail) {
		match self.last_mut() {
			Some(FormulaFragment::SuccessFailRolls(_first, rolls, _tooltip)) => {
				rolls.push(*success_fail)
			}
			_ => self.push(FormulaFragment::SuccessFailRolls(
				*success_fail,
				vec![],
				None,
			)),
		}
	}
	fn push_tooltip(&mut self, tooltip: &str) {
		match self.last_mut() {
			Some(FormulaFragment::NumberRolls(_, _, tip))
			| Some(FormulaFragment::SuccessFailRolls(_, _, tip)) => *tip = Some(tooltip.to_owned()),
			_ => (),
		}
	}
}
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
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
			if let NumberRoll::Counted(int) = number_roll {
				res += int.value();
			}
		}
		Integer::new(res)
	}
}
#[derive(Debug, Copy, Clone)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
pub enum NumberRoll {
	Counted(Integer),
	NotCounted(Integer),
}

pub type SuccessFailRolls = Vec<SuccessFail>;

#[derive(Debug, Copy, Clone)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
pub enum SuccessFail {
	Success(Integer),
	Fail(Integer),
	CriticalSuccess(Integer),
	CriticalFail(Integer),
}
