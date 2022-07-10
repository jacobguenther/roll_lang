// File: tests/mod.rs

use super::*;

#[test]
fn errors() {
	let output = helper_return_result(r, "/r (1.0)d(10)");
	println!("{:?}", output);
	match output.error {
		Some(InterpretError::DiceCountMustBeAnInteger) => (),
		Some(e) => panic!(
			"expected error InterpretError::DiceCountMustBeAnInteger got {:?}",
			e
		),
		None => panic!("expected error InterpretError::DiceCountMustBeAnInteger got none"),
	}

	let output = helper_return_result(r, "/r (10)d(1.0)");
	println!("{:?}", output);
	match output.error {
		Some(InterpretError::DiceSidesMustBeAnInteger) => (),
		Some(e) => panic!(
			"expected error InterpretError::DiceSidesMustBeAnInteger got {:?}",
			e
		),
		None => panic!("expected error InterpretError::DiceSidesMustBeAnInteger got none"),
	}

	let output = helper_return_result(r, "/r (1.0)d(1.0)");
	println!("{:?}", output);
	match output.error {
		Some(InterpretError::DiceCountMustBeAnInteger) => (),
		Some(e) => panic!(
			"expected error InterpretError::DiceCountMustBeAnInteger got {:?}",
			e
		),
		None => panic!("expected error InterpretError::DiceCountMustBeAnInteger got none"),
	}
}
