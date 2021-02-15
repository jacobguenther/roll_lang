// File: tests/modifires.rs

use super::*;

#[test]
fn errors() {
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
fn reroll() {
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
fn exploding() {
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
fn penetrating() {
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
fn compounding() {
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
fn mixed() {}
