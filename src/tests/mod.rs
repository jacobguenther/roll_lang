// File: tests/mod.rs

pub mod comments_tooltips;
pub mod computed_dice;
pub mod drop_keep;
pub mod functions;
pub mod lexer;
pub mod macros;
pub mod modifires;
pub mod sort;

use output::Output;
// use output::formats::to_string;
use std::string::ToString;

use crate::builder::InterpreterBuilder;
use crate::interpreter::*;

use crate::interpreter::error::InterpretError;
use crate::parser::error::ParseError;

use crate::macros::*;

pub fn r() -> f64 {
	0.0
}
fn helper<R: Copy + Fn() -> f64>(random_func: R, source: &str, expected_output: &str) {
	let output = InterpreterBuilder::default()
		.build(random_func)
		.interpret(&source)
		.to_string();
	println!("{}", source);
	assert_eq!(&output, expected_output);
}
fn helper_return_result<R: Copy + Fn() -> f64>(random_func: R, source: &str) -> Output {
	InterpreterBuilder::default()
		.build(random_func)
		.interpret(&source)
}

#[test]
#[allow(unused)]
fn playground() {
	// use unicode_segmentation::UnicodeSegmentation;
	// let s = "🇾🇺🇷🇸🇬🇬🇬🇭";
	// println!("{}", s);
	// println!("length: {}", s.len());
	// println!();
	// println!("chars: {}", s.chars().count());
	// for c in s.chars() {
	// 	print!("{} ", c);
	// }
	// println!("\n");
	// println!("graphemes: {}", s.graphemes(true).count());
	// for g in s.graphemes(true) {
	// 	print!("{} ", g)
	// }
	// println!("\n");
	// assert!(false);

	let r = || -> f64 {
		let nums = [0.0, 0.1, 0.3, 0.4];
		static mut I: usize = 0;
		unsafe {
			let rand = nums[I];
			I += 1;
			if I >= 4 {
				I = 0;
			}
			rand
		}
	};
	let source = "inline [[2+3]] macro #damage explicit /r 5[?test tip] + #damage \\";
	let mut macros = Macros::new();
	macros.insert("damage".to_owned(), "[[10+5]]".to_owned());
	let out = InterpreterBuilder::default()
		.with_macros(&macros)
		.build(r)
		.interpret(source);

	use output::formats::to_html::ToHtml;
	let out_str = out.to_string();

	let mut html = String::new();
	let header = "<head>
	<link rel=\"stylesheet\" href=\"styles.css\">
</head>";

	html.push_str("<html>");
	html.push_str(header);
	html.push_str("<body>");

	html.push_str("<p>input</p>");
	html.push_str(format!("<p class=\"input\">{}</p>", source).as_str());

	html.push_str("<p>output</p>");
	html.push_str(out.to_html().as_str());

	html.push_str("<p>error</p>");
	html.push_str(format!("{:?}", out.error).as_str());

	html.push_str("</body>");
	html.push_str("</html>");
	std::fs::write("index.html", html);
	assert!(true);
}

#[test]
fn interpreter() {
	// associativity
	helper(r, "[[5-4+1]]", "(2)");
	// multiply and divide
	helper(r, "[[4*6/3]]", "(8)");
	// precedence
	helper(r, "[[(4+2)*2]]", "(12)");
	// unicode and localization
	helper(r, "文字 hello", "文字 hello");
}

#[test]
fn whitespaces() {
	helper(r, "[[ 20 + 4 * 2 ]]", "(28)");

	helper(r, " [[ 20 + 4 * 2 ]]", " (28)");
	helper(r, "[[ 20 + 4 * 2 ]] ", "(28) ");

	helper(r, "text before [[ 20 + 4 * 2 ]]", "text before (28)");
	helper(r, "[[ 20 + 4 * 2 ]] text after", "(28) text after");

	helper(r, "/r 20 + 4 * 2", "20 + 4 * 2 = 28");

	helper(r, "/r 20 + 4 * 2 ", "20 + 4 * 2 = 28 ");
	helper(r, " /r 20 + 4 * 2", " 20 + 4 * 2 = 28");

	helper(r, "/r 20 + 4 * 2\\", "20 + 4 * 2 = 28");

	helper(r, "/r 20 + 4 * 2 \\", "20 + 4 * 2 = 28");
	helper(r, "/r 20 + 4 * 2 \\ ", "20 + 4 * 2 = 28 ");
}

#[test]
fn embedded_inline_roll() {
	helper(r, "/r 10+[[7+8]]", "10 + (15) = 25");
}
