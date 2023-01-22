// interpreter/output/formats/to_string.rs

use super::super::*;

use std::string::ToString;

impl ToString for Output {
	fn to_string(&self) -> String {
		let mut out_string = String::new();
		for fragment in &self.fragments {
			out_string.push_str(&fragment.to_string());
		}
		match &self.error {
			Some(error) => format!("{}::{:?}", out_string, error),
			None => out_string,
		}
	}
}

impl ToString for OutputFragment {
	fn to_string(&self) -> String {
		match self {
			OutputFragment::StringLit(s) => s.clone(),
			OutputFragment::Roll(RollType::ExplicitRoll(expression_output)) => {
				expression_output.to_string()
			}
			OutputFragment::Roll(RollType::InlineRoll(expression_output)) => {
				format!("({})", expression_output.result)
			}
		}
	}
}

impl ToString for ExpressionOutput {
	fn to_string(&self) -> String {
		let mut out_string = String::new();
		for fragment in &self.formula_fragments {
			out_string.push_str(&fragment.to_string());
		}
		format!("{} = {}", out_string, self.result)
	}
}

impl ToString for FormulaFragment {
	fn to_string(&self) -> String {
		match self {
			FormulaFragment::Basic(s, tip) => match tip {
				Some(tip) => format!("[{} | tip: {}]", s, tip),
				None => s.clone(),
			},
			FormulaFragment::NumberRolls(first, rolls, tooltip) => {
				let print_rolls = |first: &NumberRoll, rolls: &Vec<NumberRoll>| {
					let mut out_string = String::from('(');
					out_string.push_str(&first.to_string());
					for roll in rolls {
						out_string.push('+');
						out_string.push_str(&roll.to_string());
					}
					out_string.push(')');
					out_string
				};
				match tooltip {
					Some(tip) => {
						format!("[{} | tip: {}]", print_rolls(first, rolls), tip)
					}
					None => print_rolls(first, rolls),
				}
			}
			FormulaFragment::SuccessFailRolls(_, _, _) => String::from("SF"),
		}
	}
}

impl ToString for NumberRoll {
	fn to_string(&self) -> String {
		match self {
			NumberRoll::Counted(_, _) => self.value().to_string(),
			NumberRoll::NotCounted(_, _) => format!("roll_not_counted({})", self.value()),
		}
	}
}
