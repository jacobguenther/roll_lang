// File: lib.rs

extern crate unicode_segmentation;

pub mod ast;
pub mod builder;
pub mod interpreter;
pub mod lexer;
pub mod macros;
pub mod parser;

#[cfg(test)]
pub mod tests {
	use output::Output;

	use super::interpreter::*;
	use crate::builder::InterpreterBuilder;

	use crate::interpreter::error::InterpretError;
	use crate::parser::error::ParseError;

	fn r() -> f64 {
		0.0
	}
	fn helper<R: Copy + Fn() -> f64>(random_func: R, source: &str, result: &str) {
		let output = InterpreterBuilder::default()
			.with_source(&source)
			.build(random_func)
			.interpret()
			.to_string();
		println!("{}", source);
		assert_eq!(&output, result);
	}
	fn helper_return_result<R: Copy + Fn() -> f64>(random_func: R, source: &str) -> Output {
		InterpreterBuilder::default()
			.with_source(&source)
			.build(random_func)
			.interpret()
	}

	#[test]
	fn playground() {
		println!(
			"{}",
			InterpreterBuilder::default()
				.with_source(
					"I deal /r d6[spear] + [[ 3[str mod] + 4[prof bonus] ]][modifiers] \\ damage"
				)
				.build(r)
				.interpret()
				.to_string()
		);
		// assert!(false);
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
		helper(r, "[[ 20 + 4 * 2 ]] ", "(28) ");
		helper(r, "/r 20 + 4 * 2 ", "20 + 4 * 2 = 28 ");
		helper(r, "/r 20 + 4 * 2 \\", "20 + 4 * 2 = 28");
		helper(r, "/r 20 + 4 * 2 \\ ", "20 + 4 * 2 = 28 ");
	}

	#[test]
	fn comments() {
		helper(r, "/r [1]20 \\", "[1]20 = 20");
		helper(r, "/r 20[1] \\", "20[1] = 20");
		helper(r, "/r [1]20[2] \\", "[1]20[2] = 20");

		helper(r, "/r [1]1d1 \\", "[1](1) = 1");
		helper(r, "/r 1d1[1] \\", "(1)[1] = 1");
		helper(r, "/r [1]1d1[2] \\", "[1](1)[2] = 1");
	}

	#[test]
	fn tooltips() {
		helper(r, "/r [?1]20 \\", "[20 | tip: 1] = 20");
		helper(r, "/r 20[?1] \\", "[20 | tip: 1] = 20");

		helper(r, "/r [?1]1d1 \\", "[(1) | tip: 1] = 1");
		helper(r, "/r 1d1[?1] \\", "[(1) | tip: 1] = 1");
	}

	#[test]
	fn comments_and_tooltips() {
		helper(r, "/r [1][?2]20 \\", "[1][20 | tip: 2] = 20");
		helper(r, "/r [1]20[?2] \\", "[1][20 | tip: 2] = 20");
		helper(r, "/r 20[?1][2] \\", "[20 | tip: 1][2] = 20");

		helper(r, "/r [1][?2]20[3] \\", "[1][20 | tip: 2][3] = 20");
		helper(r, "/r [1]20[?2][3] \\", "[1][20 | tip: 2][3] = 20");

		helper(r, "/r [1][?2]1d1 \\", "[1][(1) | tip: 2] = 1");
		helper(r, "/r [1]1d1[?2] \\", "[1][(1) | tip: 2] = 1");
		helper(r, "/r 1d1[?1][2] \\", "[(1) | tip: 1][2] = 1");

		helper(r, "/r [1][?2]1d1[3] \\", "[1][(1) | tip: 2][3] = 1");
		helper(r, "/r [1]1d1[?2][3] \\", "[1][(1) | tip: 2][3] = 1");

		helper(r, "/r [1][?2]3d1 \\", "[1][(1+1+1) | tip: 2] = 3");
		helper(r, "/r [1]4d1[?2] \\", "[1][(1+1+1+1) | tip: 2] = 4");
		helper(r, "/r 5d1[?1][2] \\", "[(1+1+1+1+1) | tip: 1][2] = 5");
	}

	#[test]
	fn roll_modifier_errors() {
		let output = helper_return_result(r, "/r 2d10!r2!! \\");
		match output.error {
			Some(InterpretError::ParseError(
				ParseError::MultipleTypesOfExpandingModifiersNotSupported,
			)) => (),
			Some(e) => panic!(
				"expected error ParseError::MultipleTypesOfExpandingModifiersNotSupported got {:?}",
				e
			),
			None => panic!(
				"expected error ParseError::MultipleTypesOfExpandingModifiersNotSupported got none"
			),
		}

		let output = helper_return_result(r, "/r d20r>=1 \\");
		match output.error {
			Some(InterpretError::InfiniteRerollsDetected) => (),
			Some(e) => panic!(
				"expected error InterpretError::InfiniteRerollsDetected got {:?}",
				e
			),
			None => panic!("expected error InterpretError::InfiniteRerollsDetected got none"),
		}
	}

	#[test]
	fn roll_modifiers_reroll() {
		let ra = || {
			let nums = [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9];
			static mut I: usize = 0;
			unsafe {
				let rand = nums[I];
				I += 1;
				if I >= 10 {
					I = 0;
				}
				rand
			}
		};
		helper(ra, "/r 2d10r1 \\", "(roll_not_counted(1)+2+3) = 5");
		helper(ra, "/r 2d10r1 \\", "(4+5) = 9");
		helper(
			ra,
			"/r 2d10r6r7 \\",
			"(roll_not_counted(6)+roll_not_counted(7)+8+9) = 17",
		);
	}
	#[test]
	fn roll_modifiers_exploding() {
		let ra = || {
			let nums = [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9];
			static mut I: usize = 9;
			unsafe {
				let rand = nums[I];
				I += 1;
				if I >= 10 {
					I = 0;
				}
				rand
			}
		};
		helper(ra, "/r 2d10! \\", "(10+1+2) = 13");
		helper(ra, "/r 2d10!3!4 \\", "(3+4+5+6) = 18");
		helper(ra, "/r 2d10!>=8 \\", "(7+8+9+10+1) = 35");
	}
	#[test]
	fn roll_modifiers_penetrating() {
		let ra = || {
			let nums = [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9];
			static mut I: usize = 9;
			unsafe {
				let rand = nums[I];
				I += 1;
				if I >= 10 {
					I = 0;
				}
				rand
			}
		};
		helper(ra, "/r 2d10!p \\", "(10+0+2) = 12");
		helper(ra, "/r 2d10!p3!p4 \\", "(3+3+4+6) = 16");
		helper(ra, "/r 2d10!p>=8 \\", "(7+8+8+9+0) = 32");
	}

	#[test]
	fn roll_modifiers_compounding() {
		let ra = || {
			let nums = [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9];
			static mut I: usize = 9;
			unsafe {
				let rand = nums[I];
				I += 1;
				if I >= 10 {
					I = 0;
				}
				rand
			}
		};
		helper(ra, "/r 2d10!! \\", "(11+2) = 13"); // (10+1) (2)
		helper(ra, "/r 2d10!!3!!4 \\", "(12+6) = 18"); // (3+4+5) (6)
		helper(ra, "/r 2d10!!>=8 \\", "(7+28) = 35"); // (7) (8+9+10+1)
	}

	#[test]
	fn roll_modifiers_mixed() {
		// let ra = || {
		// 	let nums = [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9];
		// 	static mut I: usize = 0;
		// 	unsafe {
		// 		let rand = nums[I];
		// 		I += 1;
		// 		if I >= 10 {
		// 			I = 0;
		// 		}
		// 		rand
		// 	}
		// };
	}

	#[test]
	fn embedded_inline_roll() {
		helper(r, "/r 10+[[7+8]]", "10 + (15) = 25");
	}

	use super::macros::*;
	fn macro_helper<R: Copy + Fn() -> f64>(
		random_func: R,
		macros: &Macros,
		source: &str,
		result: &str,
	) {
		let interpreter_result = InterpreterBuilder::default()
			.with_source(&source)
			.with_macros(&macros)
			.build(random_func)
			.interpret()
			.to_string();
		assert_eq!(interpreter_result, result);
	}

	#[test]
	fn top_level_macro() {
		let mut macros = Macros::new();
		macros.insert(String::from("melee attack"), String::from("[[15+4]]"));
		macro_helper(r, &macros, "#{melee attack}", "(19)");
	}
	#[test]
	fn top_level_short_macro() {
		let mut macros = Macros::new();
		macros.insert(String::from("melee"), String::from("[[15+4]]"));
		macro_helper(r, &macros, "#melee", "(19)");
	}

	#[test]
	fn embedded_macro() {
		let mut macros = Macros::new();
		macros.insert(String::from("melee"), String::from("[[15+4]]"));
		macro_helper(r, &macros, "[[ #melee ]]", "(19)");
	}
}
