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
	pub fn as_html(&self) -> String {
		let mut out = String::new();
		for fragment in &self.fragments {
			match fragment {
				OutputFragment::StringLit(s) => {
					out = format!("{}{}",
						out,
						s
					);
				},
				OutputFragment::Roll(roll_type) => match roll_type {
					RollType::ExplicitRoll(expression_output) => {
						out = format!("{}\
								<div class=\"formula\">{}=\
								</div>\
								<div class=\"result\">{}\
								</div>",
							out,
							expression_output.formula_fragments.as_html(),
							String::from(expression_output.result)
						);
					},
					RollType::InlineRoll(expression_output) => {
						let result_string = String::from(expression_output.result);
						out = format!("{}\
								<div class=\"result tooltip\">{}\
									<div class=\"tooltiptext\">\
										<div class=\"formula\">{}=\
										</div>\
										<div class=\"result\">{}\
										</div>\
									</div>\
								</div>",
							out,
							result_string,
							expression_output.formula_fragments.as_html(),
							result_string,
						);
					}
				}
			}
		}
		match &self.error {
			Some(e) => out = format!("{} Error::{:?}", out, e.clone()),
			None => (),
		}
		out
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
	fn push_number_roll(&mut self, int: &Integer);
	fn push_success_fail_roll(&mut self, success_fail: &SuccessFail);
	fn as_html(&self) -> String;
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
	fn push_number_roll(&mut self, int: &Integer) {
		match self.last_mut() {
			Some(FormulaFragment::NumberRolls(_first, rolls)) => rolls.push(*int),
			_ => self.push(FormulaFragment::NumberRolls(*int, vec!())),
		}
	}
	fn push_success_fail_roll(&mut self, success_fail: &SuccessFail) {
		match self.last_mut() {
			Some(FormulaFragment::SuccessFailRolls(_first, rolls)) => rolls.push(*success_fail),
			_ => self.push(FormulaFragment::SuccessFailRolls(*success_fail, vec!())),
		}
	}
	fn as_html(&self) -> String {
		let mut html = String::new();
		for fragment in self {
			match fragment {
				FormulaFragment::Basic(string) => html.push_str(&string),
				FormulaFragment::NumberRolls(first_roll, rolls) => {
					html.push_str(&format!("<div class=\"basic-roll\">{}</div>", first_roll.value()));
					for roll in rolls {
						html.push_str(&format!("+<div class\"basic-roll\">{}</div>", roll.value()));
					}
				},
				_ => (),
			}
		}
		html
	}
}
#[derive(Debug, Clone)]
pub enum FormulaFragment {
	Basic(String),
	NumberRolls(Integer, Vec<Integer>),
	SuccessFailRolls(SuccessFail, Vec<SuccessFail>),
}
impl FormulaFragment {
	fn as_html(&self) -> String {
		let mut html = String::new();
		match self {
			FormulaFragment::Basic(string) => html.push_str(&string),
			FormulaFragment::NumberRolls(first_roll, rolls) => {
				let format_basic_roll = |value: &Integer| -> String {
					format!("<div class=\"basic-roll\">{}</div>", value.value())
				};

				html.push_str(&format_basic_roll(first_roll));
				for roll in rolls {
					html.push_str(&format_basic_roll(roll));
				}
			},
			FormulaFragment::SuccessFailRolls(first_roll, rolls) => {
				let format_success_fail_roll = |success_fail: &SuccessFail| -> String {
					let (class, value) = match success_fail {
						SuccessFail::Success(roll) => ("success-roll", roll.value()),
						SuccessFail::Fail(roll) => ("fail-roll", roll.value()),
						SuccessFail::CriticalSuccess(roll) => ("critical-success-roll", roll.value()),
						SuccessFail::CriticalFail(roll) => ("critical-fail-roll", roll.value()),
					};
					format!("<div class=\"{}\">{}</div>", class,  value)
				};
				html.push_str(&format_success_fail_roll(first_roll));
				for roll in rolls {
					html.push_str(&format_success_fail_roll(roll));
				}
			},
		}
		html
	}
}
#[derive(Debug, Copy, Clone)]
pub enum SuccessFail {
	Success(Integer),
	Fail(Integer),
	CriticalSuccess(Integer),
	CriticalFail(Integer),
}