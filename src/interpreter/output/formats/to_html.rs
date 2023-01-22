// interpreter/output/formtas/to_string.rs

use super::super::*;
// use super::to_string;

use std::string::ToString;

pub trait ToHtml {
	fn to_html(&self) -> String;
}

impl ToHtml for Output {
	fn to_html(&self) -> String {
		println!("{:#?}", self);
		let mut html_string = String::new();
		html_string.push_str("<div class=\"output\">");
		for fragment in self.fragments.iter() {
			let inner = fragment.to_html();
			html_string.push_str(inner.as_str());
			println!("inner {}", inner);
		}

		html_string.push_str("</div>");
		html_string
	}
}
impl ToHtml for OutputFragment {
	fn to_html(&self) -> String {
		let mut html_string = String::new();
		match self {
			OutputFragment::StringLit(s) => {
				html_string.push_str("<span>");
				html_string.push_str(s);
				html_string.push_str("</span>");
			}
			OutputFragment::Roll(RollType::ExplicitRoll(expression_output)) => {
				html_string.push_str("<div class=\"explicit_roll tooltip\">");
				let inner = expression_output.to_html();
				html_string.push_str(inner.as_str());

				html_string.push_str("<div class=\"tooltip_text text_above\">");
				html_string.push_str(expression_output.source.as_str());
				html_string.push_str("</div>");

				html_string.push_str("</div>");
			}
			OutputFragment::Roll(RollType::InlineRoll(expression_output)) => {
				html_string.push_str("<div class=\"inline_roll tooltip\">");

				html_string.push('(');
				html_string.push_str("<div class=\"roll_result\">");
				let inner = expression_output.result.to_string();
				html_string.push_str(inner.as_str());
				html_string.push_str("</div>");
				html_string.push(')');

				html_string.push_str("<div class=\"tooltip_text text_above\">");
				html_string.push_str(expression_output.source.as_str());
				html_string.push_str("</div>");

				html_string.push_str("</div>");
			}
		}
		html_string
	}
}

impl ToHtml for ExpressionOutput {
	fn to_html(&self) -> String {
		let mut out_string = String::new();
		for fragment in &self.formula_fragments {
			out_string.push_str(&fragment.to_html());
		}
		format!(
			"{} = <div class=\"roll_result\">{}</div>",
			out_string, self.result
		)
	}
}

impl ToHtml for FormulaFragment {
	fn to_html(&self) -> String {
		match self {
			FormulaFragment::Basic(s, tip) => match tip {
				Some(tip) => {
					let mut html_string = String::new();
					html_string.push_str("<div class=\"tooltip\">");
					html_string.push_str(s.as_str());

					html_string.push_str("<div class=\"tooltip_text text_below\">");
					html_string.push_str(tip);
					html_string.push_str("</div>");
					html_string.push_str("</div>");
					html_string
				}
				None => s.clone(),
			},
			FormulaFragment::NumberRolls(first, rolls, tooltip) => {
				let print_rolls = |first: &NumberRoll, rolls: &Vec<NumberRoll>| {
					let mut out_string = String::from('(');
					out_string.push_str(&first.to_html());
					for roll in rolls {
						out_string.push('+');
						out_string.push_str(&roll.to_html());
					}
					out_string.push(')');
					out_string
				};
				match tooltip {
					Some(tip) => {
						let mut html_string = String::new();
						html_string.push_str("<div class=\"number_rolls tooltip\">");
						let inner = print_rolls(first, rolls);
						html_string.push_str(inner.as_str());

						html_string.push_str("<div class=\"tooltip_text text_below\">");
						html_string.push_str(tip);
						html_string.push_str("</div>");

						html_string.push_str("</div>");
						html_string
					}
					None => print_rolls(first, rolls),
				}
			}
			FormulaFragment::SuccessFailRolls(_, _, _) => String::from("SF"),
		}
	}
}

impl ToHtml for NumberRoll {
	fn to_html(&self) -> String {
		let mut html_string = String::new();
		match self {
			NumberRoll::Counted(raw, modifier) => {
				html_string.push_str("<div class=\"number_roll\">");
				html_string.push_str(self.value().to_string().as_str());
				if let Some(modifier) = modifier {
					html_string.push_str("<div class=\"raw_roll_value\">");
					html_string.push_str(raw.to_string().as_str());
					html_string.push_str("</div>");

					html_string.push_str("<div class=\"raw_roll_value_modifier\">");
					html_string.push_str(modifier.amount.to_string().as_str());
					html_string.push_str("</div>");
				}
				html_string.push_str("</div>");
			}
			NumberRoll::NotCounted(value, _reason) => {
				html_string.push_str("<div class=\"number_roll_not_counted\">");
				html_string.push_str(value.to_string().as_str());
				html_string.push_str("</div>");
			}
		}
		html_string
	}
}
