// File: ast/number.rs

use std::ops::{Add, Sub, Mul, Div, Neg};

#[derive(Debug, Copy, Clone)]
pub enum Number {
	Integer(Integer),
	Float(Float),
}
impl Number {
	pub fn floor(&self) -> Number {
		match self {
			Number::Integer(_) => self.clone(),
			Number::Float(float) => Number::Float(Float::new(float.value().floor())),	
		}
	}
	pub fn ceil(&self) -> Number {
		match self {
			Number::Integer(int) => self.clone(),
			Number::Float(float) => Number::Float(Float::new(float.value().ceil())),	
		}
	}
	pub fn round(&self) -> Number {
		match self {
			Number::Integer(int) => self.clone(),
			Number::Float(float) => Number::Float(Float::new(float.value().round())),	
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
			(Number::Integer(b), Number::Integer(e)) =>
				(b.value() as f32, e.value() as f32),
			(Number::Integer(b), Number::Float(e)) => 
				(b.value() as f32, e.value()),
			(Number::Float(b), Number::Integer(e)) => 
				(b.value(), e.value() as f32),
			(Number::Float(b), Number::Float(e)) =>
				(b.value(), e.value())
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
			(Number::Integer(lhs_int), Number::Integer(rhs_int)) =>
				Number::Integer(lhs_int + rhs_int),
			(Number::Integer(lhs_int), Number::Float(rhs_float)) =>
				Number::Float(Float::new(lhs_int.value() as f32 + rhs_float.value())),
			(Number::Float(lhs_float), Number::Integer(rhs_int)) =>
				Number::Float(Float::new(lhs_float.value() + rhs_int.value() as f32)),
			(Number::Float(lhs_float), Number::Float(rhs_float)) =>
				Number::Float(lhs_float + rhs_float),
		}
	}

}
impl Sub for Number {
	type Output = Number;
	fn sub(self, rhs: Number) -> Self::Output {
		match (self, rhs) {
			(Number::Integer(lhs_int), Number::Integer(rhs_int)) =>
				Number::Integer(lhs_int - rhs_int),
			(Number::Integer(lhs_int), Number::Float(rhs_float)) =>
				Number::Float(Float::new(lhs_int.value() as f32 - rhs_float.value())),
			(Number::Float(lhs_float), Number::Integer(rhs_int)) =>
				Number::Float(Float::new(lhs_float.value() - rhs_int.value() as f32)),
			(Number::Float(lhs_float), Number::Float(rhs_float)) =>
				Number::Float(lhs_float - rhs_float),
		}
	}
}
impl Mul for Number {
	type Output = Number;
	fn mul(self, rhs: Number) -> Self::Output {
		match (self, rhs) {
			(Number::Integer(lhs_int), Number::Integer(rhs_int)) =>
				Number::Integer(lhs_int * rhs_int),
			(Number::Integer(lhs_int), Number::Float(rhs_float)) =>
				Number::Float(Float::new(lhs_int.value() as f32 * rhs_float.value())),
			(Number::Float(lhs_float), Number::Integer(rhs_int)) =>
				Number::Float(Float::new(lhs_float.value() * rhs_int.value() as f32)),
			(Number::Float(lhs_float), Number::Float(rhs_float)) =>
				Number::Float(lhs_float * rhs_float),
		}
	}
}
impl Div for Number {
	type Output = Result<Number, OperatorError>;
	fn div(self, divisor: Number) -> Self::Output {
		match divisor {
			Number::Integer(int) => if int.value() == 0 {
				return Err(OperatorError::DivideByZero);
			},
			Number::Float(float) => if float.value() == 0.0 {
				return Err(OperatorError::DivideByZero);
			}
		}
		match (self, divisor) {
			(Number::Integer(lhs_int), Number::Integer(rhs_int)) =>
				Ok(Number::Integer(lhs_int / rhs_int)),
			(Number::Integer(lhs_int), Number::Float(rhs_float)) =>
				Ok(Number::Float(Float::new(lhs_int.value() as f32 / rhs_float.value()))),
			(Number::Float(lhs_float), Number::Integer(rhs_int)) =>
				Ok(Number::Float(Float::new(lhs_float.value() / rhs_int.value() as f32))),
			(Number::Float(lhs_float), Number::Float(rhs_float)) =>
				Ok(Number::Float(lhs_float / rhs_float)),
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

#[derive(Debug, Copy, Clone)]
pub enum OperatorError {
	DivideByZero
}

#[derive(Debug, Copy, Clone)]
pub struct Integer {
	i: i32,
}
impl Integer {
	pub fn new(i: i32) -> Integer {
		Integer {
			i: i,
		}
	}
	pub fn value(&self) -> i32 {
		self.i
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

#[derive(Debug, Copy, Clone)]
pub struct Float {
	f: f32,
}
impl Float {
	pub fn new(f: f32) -> Float {
		Float {
			f: f,
		}
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