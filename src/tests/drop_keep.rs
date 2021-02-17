// File: tests/drop_keep.rs

use super::*;

#[test]
fn error() {
	let output = helper_return_result(r, "/r 3d4dl1kh1");
	println!("{:?}", output);
	match output.error {
		Some(InterpretError::ParseError(ParseError::MultipleDropKeepModifiersNotSupported)) => (),
		Some(e) => panic!(
			"expected error ParseError::MultipleDropKeepModifiersNotSupported got {:?}",
			e
		),
		None => panic!("expected error ParseError::MultipleDropKeepModifiersNotSupported got none"),
	}
}

#[test]
fn drop_lowest() {
	let r = || -> f64 {
		let nums = [0.0, 0.25, 0.5];
		static mut I: usize = 0;
		unsafe {
			let rand = nums[I];
			I += 1;
			if I >= 3 {
				I = 0;
			}
			rand
		}
	};
	helper(r, "/r 3d4d1", "(roll_not_counted(1)+2+3) = 5");
	helper(r, "/r 3d4dl1", "(roll_not_counted(1)+2+3) = 5");
	helper(
		r,
		"/r 3d4dl2",
		"(roll_not_counted(1)+roll_not_counted(2)+3) = 3",
	);
	let r = || -> f64 {
		let nums = [0.25, 0.0, 0.0, 0.0, 0.5];
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
	helper(
		r,
		"/r 5d4dl2",
		"(2+roll_not_counted(1)+roll_not_counted(1)+1+3) = 6",
	);
}
#[test]
fn drop_highest() {
	let r = || -> f64 {
		let nums = [0.0, 0.25, 0.5];
		static mut I: usize = 0;
		unsafe {
			let rand = nums[I];
			I += 1;
			if I >= 3 {
				I = 0;
			}
			rand
		}
	};
	helper(r, "/r 3d4dh1", "(1+2+roll_not_counted(3)) = 3");
	helper(
		r,
		"/r 3d4dh2",
		"(1+roll_not_counted(2)+roll_not_counted(3)) = 1",
	);

	let r = || -> f64 {
		let nums = [0.25, 0.5, 0.0, 0.5, 0.0, 0.5];
		static mut I: usize = 0;
		unsafe {
			let rand = nums[I];
			I += 1;
			if I >= 6 {
				I = 0;
			}
			rand
		}
	};
	helper(
		r,
		"/r 6d4dh2",
		"(2+roll_not_counted(3)+1+roll_not_counted(3)+1+3) = 7",
	);
}

#[test]
fn keep_lowest() {
	let r = || -> f64 {
		let nums = [0.0, 0.25, 0.5];
		static mut I: usize = 0;
		unsafe {
			let rand = nums[I];
			I += 1;
			if I >= 3 {
				I = 0;
			}
			rand
		}
	};
	helper(
		r,
		"/r 3d4kl1",
		"(1+roll_not_counted(2)+roll_not_counted(3)) = 1",
	);
	helper(r, "/r 3d4kl2", "(1+2+roll_not_counted(3)) = 3");

	let r = || -> f64 {
		let nums = [0.0, 0.0, 0.25];
		static mut I: usize = 0;
		unsafe {
			let rand = nums[I];
			I += 1;
			if I >= 3 {
				I = 0;
			}
			rand
		}
	};
	helper(
		r,
		"/r 3d4kl1",
		"(1+roll_not_counted(1)+roll_not_counted(2)) = 1",
	);
}
#[test]
fn keep_highest() {
	let r = || -> f64 {
		let nums = [0.0, 0.25, 0.5];
		static mut I: usize = 0;
		unsafe {
			let rand = nums[I];
			I += 1;
			if I >= 3 {
				I = 0;
			}
			rand
		}
	};
	helper(
		r,
		"/r 3d4k1",
		"(roll_not_counted(1)+roll_not_counted(2)+3) = 3",
	);
	helper(
		r,
		"/r 3d4kh1",
		"(roll_not_counted(1)+roll_not_counted(2)+3) = 3",
	);
	helper(r, "/r 3d4kh2", "(roll_not_counted(1)+2+3) = 5");
	let r = || -> f64 {
		let nums = [0.0, 0.5, 0.5];
		static mut I: usize = 0;
		unsafe {
			let rand = nums[I];
			I += 1;
			if I >= 3 {
				I = 0;
			}
			rand
		}
	};
	helper(
		r,
		"/r 3d4kh1",
		"(roll_not_counted(1)+3+roll_not_counted(3)) = 3",
	);
}
