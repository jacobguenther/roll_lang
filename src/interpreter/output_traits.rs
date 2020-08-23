// interpreter/output_traits.rs

use super::output::*;

use std::string::ToString;

pub trait AsHtml {
	fn as_html(&self) -> String;
}

impl AsHtml for Output {
	fn as_html(&self) -> String {
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
								<div class=\"result tooltipped\">{}\
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
impl AsHtml for FormulaFragments {
	fn as_html(&self) -> String {
		let mut html = String::new();
		for fragment in self {
			html = format!("{}{}", html, fragment.as_html());
		}
		html
	}
}
impl AsHtml for FormulaFragment {
	fn as_html(&self) -> String {
		let mut html = String::new();
		match self {
			FormulaFragment::Basic(string) => html.push_str(&string),
			FormulaFragment::NumberRolls(first_roll, rolls, tooltip) => {
				match tooltip {
					Some(_) => html.push_str("<div class=\"tooltipped\">"),
					None => (),
				}
				let format_basic_roll = |roll: &NumberRoll| -> String {
					match roll {
						NumberRoll::Counted(integer) => format!("<div class=\"roll-counted\">{}</div>", integer.value()),
						NumberRoll::NotCounted(integer) => format!("<div class=\"roll-not-counted\">{}</div>", integer.value())
					}
				};

				html.push_str(&format_basic_roll(first_roll));
				for roll in rolls {
					html.push_str(&format!("+{}", format_basic_roll(roll)));
				}
				match tooltip {
					Some(comment) => html.push_str(&format!("<div class=\"tooltiptext\">{}</div></div>", comment)),
					None => (),
				}
			},
			FormulaFragment::SuccessFailRolls(first_roll, rolls, _tooltip) => {
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

impl ToString for Output {
	fn to_string(&self) -> String {
		let mut out_string = String::new();
		for fragment in &self.fragments {
			out_string.push_str(&fragment.to_string());
		}
		match &self.error {
			Some(error) => format!("{} :: {:?}", out_string, error),
			None => out_string,
		}
	}
}
impl ToString for OutputFragment {
	fn to_string(&self) -> String {
		match self {
			OutputFragment::StringLit(s) => s.clone(),
			OutputFragment::Roll(RollType::ExplicitRoll(expression_output)) |
			OutputFragment::Roll(RollType::InlineRoll(expression_output)) =>
				expression_output.to_string(),
		}
	}
}
impl ToString for ExpressionOutput {
	fn to_string(&self) -> String {
		let mut out_string = String::new();
		for fragment in &self.formula_fragments {
			out_string.push_str(&fragment.to_string());
		}
		format!("{}={}", out_string, self.result)
	}
}
impl ToString for FormulaFragment {
	fn to_string(&self) -> String {
		match self {
			FormulaFragment::Basic(s) => s.clone(),
			FormulaFragment::NumberRolls(first, rolls, tooltip) => {
				let mut out_string = format!("{}", first.to_string());
				for roll in rolls {
					out_string.push_str("+");
					out_string.push_str(&roll.to_string());
				}
				out_string.push_str("");
				match tooltip {
					Some(tip) => format!("{}[{}]", out_string, tip),
					None => out_string,
				}
			}
			FormulaFragment::SuccessFailRolls(_, _, _) => String::from("SF"),
		}
	}
}
impl ToString for NumberRoll {
	fn to_string(&self) -> String {
		match self {
			NumberRoll::Counted(int) => format!("{}", int.value()),
			NumberRoll::NotCounted(_) => String::from("NC"),
		}
	}
}
