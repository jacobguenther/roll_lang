// File: ast/number.rs

#[cfg(feature = "serialize")]
use serde::{Deserialize, Serialize};

use super::error::OperatorError;
use super::{Atom, Comparison, Expression, MulDiv, Power, Unary};
use std::fmt;
use std::ops::{Add, Div, Mul, Neg, Sub};

#[derive(Debug, Copy, Clone)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
pub enum Number {
	Integer(Integer),
	Float(Float),
}
impl fmt::Display for Number {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Number::Integer(int) => write!(f, "{}", int.value()),
			Number::Float(float) => write!(f, "{}", float.value()),
		}
	}
}
impl Number {
	pub fn as_float(&self) -> Float {
		match self {
			Number::Integer(i) => Float::new(i.value() as f32),
			Number::Float(f) => *f,
		}
	}
	pub fn floor(&self) -> Number {
		match self {
			Number::Integer(_) => *self,
			Number::Float(float) => Number::Integer(Integer::new(float.value().floor() as i32)),
		}
	}
	pub fn ceil(&self) -> Number {
		match self {
			Number::Integer(_) => *self,
			Number::Float(float) => Number::Integer(Integer::new(float.value().ceil() as i32)),
		}
	}
	pub fn round(&self) -> Number {
		match self {
			Number::Integer(_) => *self,
			Number::Float(float) => Number::Integer(Integer::new(float.value().round() as i32)),
		}
	}
	pub fn round_half_down(&self) -> Number {
		match self {
			Number::Integer(_) => *self,
			Number::Float(float) => {
				let value = float.value();
				let lower = self.floor().as_float().value();
				if value - lower == 0.5 {
					Number::Float(Float::new(lower))
				} else {
					self.ceil()
				}
			}
		}
	}
	pub fn abs(&self) -> Number {
		match self {
			Number::Integer(int) => Number::Integer(Integer::new(int.value().abs())),
			Number::Float(float) => Number::Float(Float::new(float.value().abs())),
		}
	}
	pub fn pow(&self, exponent: &Number) -> Number {
		let (b, e) = match (self, exponent) {
			(Number::Integer(b), Number::Integer(e)) => (b.value() as f32, e.value() as f32),
			(Number::Integer(b), Number::Float(e)) => (b.value() as f32, e.value()),
			(Number::Float(b), Number::Integer(e)) => (b.value(), e.value() as f32),
			(Number::Float(b), Number::Float(e)) => (b.value(), e.value()),
		};
		Number::Float(Float::new(b.powf(e)))
	}
}
impl From<Number> for String {
	fn from(num: Number) -> String {
		match num {
			Number::Integer(int) => int.value().to_string(),
			Number::Float(float) => float.value().to_string(),
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
				Number::Float(Float::new(lhs_int.value() as f32 + rhs_float.value()))
			}
			(Number::Float(lhs_float), Number::Integer(rhs_int)) => {
				Number::Float(Float::new(lhs_float.value() + rhs_int.value() as f32))
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
				Number::Float(Float::new(lhs_int.value() as f32 - rhs_float.value()))
			}
			(Number::Float(lhs_float), Number::Integer(rhs_int)) => {
				Number::Float(Float::new(lhs_float.value() - rhs_int.value() as f32))
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
				Number::Float(Float::new(lhs_int.value() as f32 * rhs_float.value()))
			}
			(Number::Float(lhs_float), Number::Integer(rhs_int)) => {
				Number::Float(Float::new(lhs_float.value() * rhs_int.value() as f32))
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
				if int.value() == 0 {
					return Err(OperatorError::DivideByZero);
				}
			}
			Number::Float(float) => {
				if float.value() == 0.0 {
					return Err(OperatorError::DivideByZero);
				}
			}
		}
		match (self, divisor) {
			(Number::Integer(lhs_int), Number::Integer(rhs_int)) => {
				Ok(Number::Integer(lhs_int / rhs_int))
			}
			(Number::Integer(lhs_int), Number::Float(rhs_float)) => Ok(Number::Float(Float::new(
				lhs_int.value() as f32 / rhs_float.value(),
			))),
			(Number::Float(lhs_float), Number::Integer(rhs_int)) => Ok(Number::Float(Float::new(
				lhs_float.value() / rhs_int.value() as f32,
			))),
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

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
pub struct Integer {
	i: i32,
}
impl Integer {
	pub fn new(i: i32) -> Integer {
		Integer { i }
	}
	pub fn value(&self) -> i32 {
		self.i
	}
	pub fn comparison(&self, comparison: &Comparison, rhs: &Integer) -> bool {
		let lhs_value = self.i;
		let rhs_value = rhs.i;
		match comparison {
			Comparison::LessThan => lhs_value < rhs_value,
			Comparison::GreaterThan => lhs_value > rhs_value,
			Comparison::LessThanEqual => lhs_value <= rhs_value,
			Comparison::GreaterThanEqual => lhs_value >= rhs_value,
			Comparison::Equal => lhs_value == rhs_value,
		}
	}
	pub fn clamp_min(&self, min: i32) -> Integer {
		Integer::new(if self.i < min { min } else { self.i })
	}
	pub fn as_expression(&self) -> Expression {
		let new_int = Integer::new(self.i);
		Expression::MulDiv(MulDiv::Power(Power::Unary(Unary::Atom(
			None,
			Atom::Number(Number::Integer(new_int), None),
			None,
		))))
	}
}
impl Add for Integer {
	type Output = Integer;
	fn add(self, other: Integer) -> Self::Output {
		Integer::new(self.i + other.i)
	}
}
impl Sub for Integer {
	type Output = Integer;
	fn sub(self, other: Integer) -> Self::Output {
		Integer::new(self.i - other.i)
	}
}
impl Div for Integer {
	type Output = Integer;
	fn div(self, other: Integer) -> Self::Output {
		Integer::new(self.i / other.i)
	}
}
impl Mul for Integer {
	type Output = Integer;
	fn mul(self, other: Integer) -> Self::Output {
		Integer::new(self.i * other.i)
	}
}
impl Neg for Integer {
	type Output = Integer;
	fn neg(self) -> Self::Output {
		Integer::new(-self.i)
	}
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
pub struct Float {
	f: f32,
}
impl Float {
	pub fn new(f: f32) -> Float {
		Float { f }
	}
	pub fn value(&self) -> f32 {
		self.f
	}
}
impl Add for Float {
	type Output = Float;
	fn add(self, other: Float) -> Self::Output {
		Float::new(self.f + other.f)
	}
}
impl Sub for Float {
	type Output = Float;
	fn sub(self, other: Float) -> Self::Output {
		Float::new(self.f - other.f)
	}
}
impl Div for Float {
	type Output = Float;
	fn div(self, other: Float) -> Self::Output {
		Float::new(self.f / other.f)
	}
}
impl Mul for Float {
	type Output = Float;
	fn mul(self, other: Float) -> Self::Output {
		Float::new(self.f * other.f)
	}
}
impl Neg for Float {
	type Output = Float;
	fn neg(self) -> Self::Output {
		Float::new(-self.f)
	}
}
