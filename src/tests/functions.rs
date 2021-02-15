// File: tests/functions.rs

use super::*;

#[test]
fn abs() {
	helper(r, "[[abs(0)]]", "(0)");
	helper(r, "[[abs(0.0)]]", "(0)");

	helper(r, "[[abs(1)]]", "(1)");
	helper(r, "[[abs(-1)]]", "(1)");

	helper(r, "[[abs(2.0)]]", "(2)");
	helper(r, "[[abs(-2.0)]]", "(2)");
}

#[test]
fn ceil() {
	helper(r, "[[ceil(1)]]", "(1)");
	helper(r, "[[ceil(1.0)]]", "(1)");
	helper(r, "[[ceil(1.1)]]", "(2)");
	helper(r, "[[ceil(1.5)]]", "(2)");
	helper(r, "[[ceil(1.9)]]", "(2)");
	helper(r, "[[ceil(2)]]", "(2)");
	helper(r, "[[ceil(2.0)]]", "(2)");
}

#[test]
fn floor() {
	helper(r, "[[floor(1)]]", "(1)");
	helper(r, "[[floor(1.0)]]", "(1)");
	helper(r, "[[floor(1.1)]]", "(1)");
	helper(r, "[[floor(1.5)]]", "(1)");
	helper(r, "[[floor(1.9)]]", "(1)");
	helper(r, "[[floor(2)]]", "(2)");
	helper(r, "[[floor(2.0)]]", "(2)");
}

#[test]
fn round() {
	helper(r, "[[round(1)]]", "(1)");
	helper(r, "[[round(1.0)]]", "(1)");
	helper(r, "[[round(1.5)]]", "(2)");
	helper(r, "[[round(1.9)]]", "(2)");
}

#[test]
fn round_half_down() {
	helper(r, "[[round_half_down(1)]]", "(1)");
	helper(r, "[[round_half_down(1.0)]]", "(1)");
	helper(r, "[[round_half_down(1.5)]]", "(1)");
	helper(r, "[[round_half_down(1.6)]]", "(2)");
}
