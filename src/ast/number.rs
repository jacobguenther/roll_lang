// File: ast/number.rs

use super::error::OperatorError;
use super::{Atom, Comparison, Expression, MulDiv, Power, Unary};
use std::fmt;
use std::ops::{Add, Div, Mul, Neg, Sub};

pub type Integer = i64;
pub type Float = f64;

#[derive(Debug, Copy, Clone)]
pub enum Number {
	Integer(Integer),
	Float(Float),
}
impl fmt::Display for Number {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Number::Integer(int) => write!(f, "{}", int),
			Number::Float(float) => write!(f, "{}", float),
		}
	}
}
impl Number {
	pub fn as_float(&self) -> Float {
		match self {
			Number::Integer(i) => *i as Float,
			Number::Float(f) => *f,
		}
	}
	pub fn floor(&self) -> Number {
		match self {
			Number::Integer(_) => *self,
			Number::Float(float) => Number::Integer(float.floor() as Integer),
		}
	}
	pub fn ceil(&self) -> Number {
		match self {
			Number::Integer(_) => *self,
			Number::Float(float) => Number::Integer(float.ceil() as Integer),
		}
	}
	pub fn round(&self) -> Number {
		match self {
			Number::Integer(_) => *self,
			Number::Float(float) => Number::Integer(float.round() as Integer),
		}
	}
	pub fn round_half_down(&self) -> Number {
		match self {
			Number::Integer(_) => *self,
			Number::Float(value) => {
				let lower = self.floor().as_float();
				if ((*value - lower) - 0.5).abs() < Float::EPSILON {
					Number::Float(lower)
				} else {
					self.ceil()
				}
			}
		}
	}
	pub fn abs(&self) -> Number {
		match self {
			Number::Integer(int) => Number::Integer(int.abs()),
			Number::Float(float) => Number::Float(float.abs()),
		}
	}
	pub fn pow(&self, exponent: &Number) -> Number {
		let (b, e) = match (self, exponent) {
			(Number::Integer(b), Number::Integer(e)) => (*b as Float, *e as Float),
			(Number::Integer(b), Number::Float(e)) => (*b as Float, *e),
			(Number::Float(b), Number::Integer(e)) => (*b, *e as Float),
			(Number::Float(b), Number::Float(e)) => (*b, *e),
		};
		Number::Float(b.powf(e))
	}
}
impl From<Number> for String {
	fn from(num: Number) -> String {
		match num {
			Number::Integer(int) => int.to_string(),
			Number::Float(float) => float.to_string(),
		}
	}
}
impl Add for Number {
	type Output = Number;
	fn add(self, rhs: Number) -> Self::Output {
		match (self, rhs) {
			(Number::Integer(lhs_int), Number::Integer(rhs_int)) => {
				Number::Integer(lhs_int + rhs_int)
			}
			(Number::Integer(lhs_int), Number::Float(rhs_float)) => {
				Number::Float(lhs_int as Float + rhs_float)
			}
			(Number::Float(lhs_float), Number::Integer(rhs_int)) => {
				Number::Float(lhs_float + rhs_int as Float)
			}
			(Number::Float(lhs_float), Number::Float(rhs_float)) => {
				Number::Float(lhs_float + rhs_float)
			}
		}
	}
}
impl Sub for Number {
	type Output = Number;
	fn sub(self, rhs: Number) -> Self::Output {
		match (self, rhs) {
			(Number::Integer(lhs_int), Number::Integer(rhs_int)) => {
				Number::Integer(lhs_int - rhs_int)
			}
			(Number::Integer(lhs_int), Number::Float(rhs_float)) => {
				Number::Float(lhs_int as Float - rhs_float)
			}
			(Number::Float(lhs_float), Number::Integer(rhs_int)) => {
				Number::Float(lhs_float - rhs_int as Float)
			}
			(Number::Float(lhs_float), Number::Float(rhs_float)) => {
				Number::Float(lhs_float - rhs_float)
			}
		}
	}
}
impl Mul for Number {
	type Output = Number;
	fn mul(self, rhs: Number) -> Self::Output {
		match (self, rhs) {
			(Number::Integer(lhs_int), Number::Integer(rhs_int)) => {
				Number::Integer(lhs_int * rhs_int)
			}
			(Number::Integer(lhs_int), Number::Float(rhs_float)) => {
				Number::Float(lhs_int as Float * rhs_float)
			}
			(Number::Float(lhs_float), Number::Integer(rhs_int)) => {
				Number::Float(lhs_float * rhs_int as Float)
			}
			(Number::Float(lhs_float), Number::Float(rhs_float)) => {
				Number::Float(lhs_float * rhs_float)
			}
		}
	}
}
impl Div for Number {
	type Output = Result<Number, OperatorError>;
	fn div(self, divisor: Number) -> Self::Output {
		match divisor {
			Number::Integer(int) => {
				if int == 0 {
					return Err(OperatorError::DivideByZero);
				}
			}
			Number::Float(float) => {
				if float == 0.0 {
					return Err(OperatorError::DivideByZero);
				}
			}
		}
		match (self, divisor) {
			(Number::Integer(lhs_int), Number::Integer(rhs_int)) => {
				Ok(Number::Integer(lhs_int / rhs_int))
			}
			(Number::Integer(lhs_int), Number::Float(rhs_float)) => {
				Ok(Number::Float(lhs_int as Float / rhs_float))
			}
			(Number::Float(lhs_float), Number::Integer(rhs_int)) => {
				Ok(Number::Float(lhs_float / rhs_int as Float))
			}
			(Number::Float(lhs_float), Number::Float(rhs_float)) => {
				Ok(Number::Float(lhs_float / rhs_float))
			}
		}
	}
}
impl Neg for Number {
	type Output = Number;
	fn neg(self) -> Self::Output {
		match self {
			Number::Integer(int) => Number::Integer(-int),
			Number::Float(float) => Number::Float(-float),
		}
	}
}

pub fn integer_as_expression(i: Integer) -> Expression {
	Expression::MulDiv(MulDiv::Power(Power::Unary(Unary::Atom(
		None,
		Box::new(Atom::Number(Number::Integer(i), None)),
		None,
		None,
	))))
}
pub fn compare_integers(comparison: &Comparison, lhs: Integer, rhs: Integer) -> bool {
	match comparison {
		Comparison::LessThan => lhs < rhs,
		Comparison::GreaterThan => lhs > rhs,
		Comparison::LessThanEqual => lhs <= rhs,
		Comparison::GreaterThanEqual => lhs >= rhs,
		Comparison::Equal => lhs == rhs,
	}
}
