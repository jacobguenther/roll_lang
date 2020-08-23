// interpreter/output_traits.rs

use super::output::*;

use std::string::ToString;

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
