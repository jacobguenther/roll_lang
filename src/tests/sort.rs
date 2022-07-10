// File: tests/modifires.rs

use super::*;

#[test]
fn errors() {
	let output = helper_return_result(r, "/r 2d10sasd \\");
	match output.error {
		Some(InterpretError::ParseError(ParseError::MultipleSortModifiersNotSupported)) => (),
		Some(e) => panic!(
			"expected error ParseError::MultipleSortModifiersNotSupported got {:?}",
			e
		),
		None => panic!(
			"expected error ParseError::MultipleSortModifiersNotSupported got: None output: {}",
			output.to_string()
		),
	}
}

#[test]
fn sort_ascending() {
	let ra = || {
		let nums = [0.0, 0.0, 0.4, 0.4, 0.1, 0.3, 0.2];
		static mut I: usize = 0;
		unsafe {
			let rand = nums[I];
			I += 1;
			if I >= 7 {
				I = 0;
			}
			rand
		}
	};
	helper(ra, "/r 7d10sa \\", "(1+1+2+3+4+5+5) = 21");
	helper(ra, "/r 7d10s \\", "(1+1+2+3+4+5+5) = 21");
}
#[test]
fn sort_descending() {
	let ra = || {
		let nums = [0.0, 0.0, 0.4, 0.4, 0.1, 0.3, 0.2];
		static mut I: usize = 0;
		unsafe {
			let rand = nums[I];
			I += 1;
			if I >= 7 {
				I = 0;
			}
			rand
		}
	};
	helper(ra, "/r 7d10sd \\", "(5+5+4+3+2+1+1) = 21");
}
#[test]
fn not_counted() {
	let ra = || {
		let nums = [0.0, 0.1, 0.1, 0.4, 0.4, 0.1, 0.3, 0.2];
		static mut I: usize = 0;
		unsafe {
			let rand = nums[I];
			I += 1;
			if I >= 8 {
				I = 0;
			}
			rand
		}
	};
	helper(
		ra,
		"/r 7d10sdr1 \\",
		"(5+5+4+3+2+2+2+roll_not_counted(1)) = 23",
	);
	helper(
		ra,
		"/r 7d10r1sa \\",
		"(roll_not_counted(1)+2+2+2+3+4+5+5) = 23",
	);

	let ra = || {
		let nums = [0.2, 0.1, 0.0, 0.4, 0.2];
		static mut I: usize = 0;
		unsafe {
			let rand = nums[I];
			I += 1;
			if I >= 5 {
				I = 0;
			}
			rand
		}
	};
	helper(ra, "/r 4d10r2sa \\", "(1+roll_not_counted(2)+3+3+5) = 12");
}
