// File: parser.rs

use super::lexer::{
	Lexer,
	LexerT,
	Lexeme,
	Token,
	TokenT,
};

use ast::*;

pub trait ParserT {
	fn new(source: &str) -> Parser;
	fn parse(&mut self) -> Root;
}
trait ParserPrivateT {
	fn parse_start(&mut self);

	fn parse_string_literal(&mut self) -> StringLiteral;

	fn parse_expression(&mut self) -> Result<Expression, ParseError>;
	fn parse_term(&mut self) -> Result<Term, ParseError>;
	fn parse_factor(&mut self) -> Result<Factor, ParseError>;
	fn parse_exponent(&mut self) -> Result<Exponent, ParseError>;
	fn parse_function(&mut self) -> Result<Function, ParseError>;
	fn parse_number(&mut self) -> Result<Number, ParseError>;

	fn parse_integer(&mut self) -> Result<u32, ParseError>;
	fn parse_float(&mut self) -> Result<f32, ParseError>;

	fn parse_dice(&mut self) -> Result<Dice, ParseError>;
	fn parse_normal(&mut self) -> Result<Normal, ParseError>;
	fn parse_fate(&mut self) -> Result<Fate, ParseError>;
	fn parse_computed(&mut self) -> Result<Computed, ParseError>;
	fn parse_modifiers(&mut self) -> Vec<Modifier>;
	fn parse_comparison(&mut self) -> Result<Comparison, ParseError>;
	fn parse_comparison_and_integer(&mut self) -> Result<(Comparison, Option<u32>), ParseError>;
	fn parse_require_comparison_and_require_integer(&mut self) -> Result<(Comparison, u32), ParseError>;
	fn parse_comparison_and_require_integer(&mut self) -> Result<(Comparison, u32), ParseError>;
	fn parse_exploding(&mut self) -> Result<Modifier, ParseError>;
	fn parse_compounding(&mut self) -> Result<Modifier, ParseError>;
	fn parse_high_low(&mut self) -> Result<Modifier, ParseError>;
	fn parse_reroll(&mut self) -> Result<Modifier, ParseError>;
	fn parse_successes(&mut self) -> Result<Modifier, ParseError>;
	fn parse_cirtical(&mut self) -> Result<Modifier, ParseError>;

	fn step_lexemes(&mut self, n: usize);

	fn current(&self) -> Result<&Lexeme, ParseError>;
	fn next(&self) -> Result<&Lexeme, ParseError>;
	fn current_as_option(&self) -> Option<&Lexeme>;
	fn next_as_option(&self) -> Option<&Lexeme>;

	fn is_what_punctuation(&self, lexeme: &Lexeme, punctuation: &str) -> Result<Token, ParseError>;
	fn is_what_literal(&self, lexeme: &Lexeme, literal: &str) -> Result<Token, ParseError>;
	fn is_what_operator(&self, lexeme: &Lexeme, operator: &str) -> Result<Token, ParseError>;

	fn is_roll(&self) -> bool;
	fn is_inline_roll(&self) -> bool;
	fn is_end_of_roll(&self) -> bool;
}

#[derive(Debug, Clone)]
pub enum ParseError {
	UnexpectedToken(Token),
	ExpectedInteger,
	DoesNotMatch,
	OutOfBounds,
	Unknown,
}
#[derive(Debug, Copy, Clone)]
enum Roll {
	Explicit,
	Inline,
}
#[derive(Debug, Clone)]
enum State {
	Start,
	StringLiteral,
	Roll(Roll),
	Done,
}
impl Default for State {
	fn default() -> State {
		State::Start
	}
}
pub struct Parser {
	state: State,
	root: Root,
	
	lexemes: Vec<Lexeme>,
	current_index: usize,
}
impl ParserT for Parser {
	fn new(source: &str) -> Parser {
		Parser {
			state: State::default(),
			root: Vec::new(),
			lexemes: Lexer::new(source).collect(),
			current_index: 0,
		}
	}
	fn parse(&mut self) -> Root {
		loop {
			match &self.state {
				State::Start => self.parse_start(),
				State::StringLiteral => {
					let literal = self.parse_string_literal();
					self.root.push(Node::StringLiteral(literal));
				},
				State::Roll(_) => {
					let result = self.parse_expression();
					match result {
						Ok(expression) => self.root.push(Node::Expression(expression)),
						Err(e) => {
							self.root.push(Node::ParseError(e.clone()));
							self.state = State::Done;
						},
					};
				},
				State::Done => break,
			};
		}
		self.root.clone()
	}
}
impl ParserPrivateT for Parser {
	fn parse_start(&mut self) {
		self.state = if self.is_roll() {
			self.step_lexemes(2);
			State::Roll(Roll::Inline)
		} else if self.is_inline_roll() {
			self.step_lexemes(2);
			State::Roll(Roll::Explicit)
		} else if self.current().is_err() {
			State::Done
		} else {
			State::StringLiteral
		};
	}

	fn parse_string_literal(&mut self) -> StringLiteral {
		let mut literal = StringLiteral::default();
		loop {
			literal.append(self.current().unwrap().source());
			self.step_lexemes(1);
			if self.is_roll() {
				self.step_lexemes(2);
				self.state = State::Roll(Roll::Explicit);
				break;
			} else if self.is_inline_roll() {
				self.step_lexemes(2);
				self.state = State::Roll(Roll::Inline);
				break;
			} else if self.current().is_err() {
				self.state = State::Done;
				break;
			}
		}
		literal
	}

	fn parse_expression(&mut self) -> Result<Expression, ParseError> {
		super::log("0 expression");
		self.state = State::Done;
		let mut expression = Expression::Term(self.parse_term()?);
		loop {
			let start_index = self.current_index;
			let is_add = match self.current() {
				Ok(lexeme) => match lexeme {
					Lexeme::Operator(token) => match token.source() {
						"+" => true,
						"-" => false,
						_ => break, // wrong operator
					},
					_ => break, // not an operator
				},
				Err(_e) => break, // probably at EOF
			};
			self.step_lexemes(1);
			let next_term = match self.parse_term() {
				Ok(t) => t,
				Err(_e) => {
					self.current_index = start_index;
					break;
				},
			};
			expression = match is_add {
				true => Expression::Add(Box::new(expression), next_term),
				false => Expression::Subtract(Box::new(expression), next_term),
			};
		}
		Ok(expression)
	}
	fn parse_term(&mut self) -> Result<Term, ParseError> {
		super::log("1 term");
		let mut term = Term::Factor(self.parse_factor()?);
		loop {
			let start_index = self.current_index;
			let is_multiply = match self.current() {
				Ok(lexeme) => match lexeme {
					Lexeme::Operator(token) => match token.source() {
						"*" => true,
						"/" => false,
						_ => break,
					},
					_ => break,
				},
				Err(_e) => break,
			};

			self.step_lexemes(1);
			let next_factor = match self.parse_factor() {
				Ok(t) => t,
				Err(_e) => {
					self.current_index = start_index;
					break;
				},
			};
			term = match is_multiply {
				true => Term::Multiply(Box::new(term), next_factor),
				false => Term::Divide(Box::new(term), next_factor),
			};
		}
		Ok(term)
	}
	fn parse_factor(&mut self) -> Result<Factor, ParseError> {
		super::log("2 factor");
		let lhs = self.parse_exponent()?;
		match self.current() {
			Ok(lexeme) => match lexeme {
				Lexeme::Operator(token) => match token.source() {
					"**" | "^" => {
						let start_index = self.current_index;
						self.step_lexemes(1);
						match self.parse_factor() {
							Ok(factor) => return Ok(Factor::Pow(lhs, Box::new(factor))),
							Err(_parse_error) => self.current_index = start_index,
						};
					},
					_ => (),
				},
				_ => (),
			},
			Err(_e) => (),
		};
		Ok(Factor::Exponent(lhs))
	}
	fn parse_exponent(&mut self) -> Result<Exponent, ParseError> {
		super::log("3 exponent");
		let start_index = self.current_index;
		match self.parse_dice() {
			Ok(dice) => return Ok(Exponent::Dice(dice)),
			Err(_e) => self.current_index = start_index,
		};
		match self.parse_number() {
			Ok(num) => return Ok(Exponent::Number(num)),
			Err(_e) => self.current_index = start_index,
		};
		match self.parse_function() {
			Ok(function) => return Ok(Exponent::Function(function)),
			Err(_e) => self.current_index = start_index,
		};
		self.is_what_punctuation(self.current()?, "(")?;
		let expression = Box::new(self.parse_expression()?);
		self.is_what_punctuation(self.current()?, ")")?;
		Ok(Exponent::Expression(expression))
	}
	fn parse_function(&mut self) -> Result<Function, ParseError> {
		super::log("4 function");
		match self.current()?.clone() {
			Lexeme::Literal(token) => match token.source() {
				"abs" | "ceil" | "floor"| "round" => {
					self.step_lexemes(1);
					self.is_what_punctuation(self.current()?, "(")?;
					self.step_lexemes(1);

					let expression = Box::new(self.parse_expression()?);
					let function = match token.source() {
						"abs" => Function::Abs(expression),
						"ceil" => Function::Ceil(expression),
						"floor" => Function::Floor(expression),
						"round" => Function::Round(expression),
						_ => return Err(ParseError::Unknown),
					};
					
					self.is_what_punctuation(self.current()?, ")")?;
					self.step_lexemes(1);
					Ok(function)
				},
				_ => Err(ParseError::DoesNotMatch),
			},
			_ => Err(ParseError::DoesNotMatch),
		}
	}

	fn parse_number(&mut self) -> Result<Number, ParseError> {
		super::log("4 number");
		let start_index = self.current_index;
		match self.parse_float() {
			Ok(float) => return Ok(Number::Float(float)),
			Err(_e) => self.current_index = start_index,
		};
		match self.parse_integer() {
			Ok(int) => Ok(Number::Integer(int)),
			Err(_e) => Err(ParseError::DoesNotMatch),
		}
	}
	fn parse_float(&mut self) -> Result<f32, ParseError> {
		super::log("5 float");
		match self.current()?.clone() {
			Lexeme::Number(integer_token) => {
				self.step_lexemes(1);
				self.is_what_punctuation(self.current()?, ".")?;
				self.step_lexemes(1);
				let integer_source = integer_token.source();
				match self.current()? {
					Lexeme::Number(fraction_token) => {
						let fraction_source = fraction_token.source();
						Ok(format!("{}.{}", integer_source, fraction_source)
							.parse().unwrap()
						)
					},
					_ => Err(ParseError::DoesNotMatch)
				}
			},
			_ => Err(ParseError::DoesNotMatch)
		}
	}
	fn parse_integer(&mut self) -> Result<u32, ParseError> {
		super::log("5 integer");
		match self.current()?.clone() {
			Lexeme::Number(integer_token) => {
				self.step_lexemes(1);
				Ok(integer_token.source().parse().unwrap())
			},
			_ => Err(ParseError::DoesNotMatch)
		}
	}
	fn parse_dice(&mut self) -> Result<Dice, ParseError> {
		super::log("5 dice");
		let start_index = self.current_index;
		match self.parse_normal() {
			Ok(normal) => return Ok(Dice::Normal(normal, self.parse_modifiers())),
			Err(_e) => self.current_index = start_index,
		};
		match self.parse_fate() {
			Ok(fate) => return Ok(Dice::Fate(fate, self.parse_modifiers())),
			Err(_e) => self.current_index = start_index,
		};
		match self.parse_computed() {
			Ok(computed) => return Ok(Dice::Computed(computed, self.parse_modifiers())),
			Err(_e) => self.current_index = start_index,
		}
		Err(ParseError::DoesNotMatch)
	}
	fn parse_normal(&mut self) -> Result<Normal, ParseError> {
		let count = self.parse_integer()?;		
		self.is_what_literal(self.current()?, "d")?;
		self.step_lexemes(1);
		let sides = self.parse_integer()?;
		Ok(Normal {count: count, sides: sides})
	}
	fn parse_fate(&mut self) -> Result<Fate, ParseError> {
		let count = self.parse_integer()?;		
		self.is_what_literal(self.current()?, "dF")?;
		self.step_lexemes(1);
		Ok(Fate {count: count})
	}
	fn parse_computed(&mut self) -> Result<Computed, ParseError> {
		// self.is_what_punctuation(self.current()?, "(")?;
		// self.step_lexemes(1);
		// super::log(&format!("parsing expression for count: {:?}", self.current()?));
		// let count = self.parse_expression()?;
		// super::log(&format!("COUNT {:?}", count));
		// self.is_what_punctuation(self.current()?, ")")?;
		// self.step_lexemes(1);
		// self.is_what_literal(self.current()?, "d")?;
		// self.step_lexemes(1);
		// let sides = self.parse_expression()?;
		// Ok(Computed {count: Box::new(count), sides: Box::new(sides)})
		Err(ParseError::DoesNotMatch)
	}
	fn parse_modifiers(&mut self) -> Vec<Modifier> {
		let mut modifiers = Vec::new();

		loop {
			let mut found_one = false;
			let start_index = self.current_index;
			match self.parse_exploding() {
				Ok(exploding_modifier) => {
					modifiers.push(exploding_modifier);
					found_one = true;
				},
				Err(_parse_error) => self.current_index = start_index,
			};
			let start_index = self.current_index;
			match self.parse_compounding() {
				Ok(compounding_modifier) => {
					modifiers.push(compounding_modifier);
					found_one = true;
				},
				Err(_parse_error) => self.current_index = start_index,
			};
			let start_index = self.current_index;
			match self.parse_reroll() {
				Ok(reroll_modifier) => {
					modifiers.push(reroll_modifier);
					found_one = true;
				},
				Err(_parse_error) => self.current_index = start_index,
			};
			let start_index = self.current_index;
			match self.parse_successes() {
				Ok(successes_modifier) => {
					modifiers.push(successes_modifier);
					found_one = true;
				},
				Err(_parse_error) => self.current_index = start_index,
			};
			let start_index = self.current_index;
			match self.parse_cirtical() {
				Ok(critical_modifier) => {
					modifiers.push(critical_modifier);
					found_one = true;
				},
				Err(_parse_error) => self.current_index = start_index,
			};
			if !found_one {
				break;
			}
		}
		modifiers
	}
	fn parse_comparison(&mut self) -> Result<Comparison, ParseError> {
		match self.current()? {
			Lexeme::Comparison(token) => match token.source() {
				"<" => Ok(Comparison::LessThan),
				">" => Ok(Comparison::GreaterThan),
				"<=" => Ok(Comparison::LessThanEqual),
				">=" => Ok(Comparison::GreaterThanEqual),
				"=" => Ok(Comparison::Equal),
				_ => Err(ParseError::DoesNotMatch),
			},
			_ => Err(ParseError::DoesNotMatch),
		}
	}
	fn parse_comparison_and_integer(&mut self) -> Result<(Comparison, Option<u32>), ParseError> {
		match self.parse_comparison() {
			Ok(comparison) => {
				let start_index = self.current_index;
				self.step_lexemes(1);
				match self.parse_integer() {
					Ok(integer) => Ok((comparison, Some(integer))),
					Err(_parse_error) => {
						self.current_index = start_index;
						Err(ParseError::ExpectedInteger)
					}
				}
			},
			Err(_parse_error) => Ok((Comparison::Equal, self.parse_integer().ok())),
		}
	}
	fn parse_comparison_and_require_integer(&mut self) -> Result<(Comparison, u32), ParseError> {
		let start_index = self.current_index;
		let comparison = match self.parse_comparison() {
			Ok(comparison) => {
				self.step_lexemes(1);
				comparison
			},
			Err(_parse_error) => Comparison::Equal,
		};
		match self.parse_integer() {
			Ok(integer) => Ok((comparison, integer)),
			Err(_parse_error) => Err(ParseError::ExpectedInteger),
		}
	}
	fn parse_require_comparison_and_require_integer(&mut self) -> Result<(Comparison, u32), ParseError> {
		let start_index = self.current_index;
		let comparison = self.parse_comparison()?;
		self.step_lexemes(1);
		match self.parse_integer() {
			Ok(integer) => Ok((comparison, integer)),
			Err(_parse_error) => Err(ParseError::ExpectedInteger),
		}
	}

	fn parse_exploding(&mut self) -> Result<Modifier, ParseError> {
		self.is_what_operator(self.current()?, "!")?;
		let start_index = self.current_index;
		self.step_lexemes(1);
		let (comparison, number) = self.parse_comparison_and_integer()?;
		Ok(Modifier::Exploding(comparison, number))
	}
	fn parse_compounding(&mut self) -> Result<Modifier, ParseError> {
		self.is_what_operator(self.current()?, "!!")?;
		let start_index = self.current_index;
		self.step_lexemes(1);
		let (comparison, number) = self.parse_comparison_and_integer()?;
		Ok(Modifier::Compounding(comparison, number))
	}
	fn parse_high_low(&mut self) -> Result<Modifier, ParseError> {
		match self.current()?.clone() {
			Lexeme::Literal(token) => match token.source() {
				"dh" | "k" | "kh" |
				"d" | "dl" | "kl" => {
					let start_index = self.current_index;
					self.step_lexemes(1);
					let count = match self.parse_integer() {
						Ok(int) => int,
						Err(_parse_error) => {
							self.current_index = start_index;
							return Err(ParseError::ExpectedInteger);
						}
					};
					match token.source() {
						"dh" => Ok(Modifier::DropHighest(count)),
						"k" | "kh" => Ok(Modifier::KeepHighest(count)),
						"d" | "dl" => Ok(Modifier::DropLowest(count)),
						"kl" => Ok(Modifier::KeepLowest(count)),
						_ => Err(ParseError::Unknown),
					}
				},
				_ => Err(ParseError::DoesNotMatch),
			},
			_ => Err(ParseError::DoesNotMatch),
		}
	}
	fn parse_reroll(&mut self) -> Result<Modifier, ParseError> {
		self.is_what_literal(self.current()?, "r")?;
		let start_index = self.current_index;
		self.step_lexemes(1);
		match self.parse_comparison_and_require_integer() {
			Ok((comparison, integer)) => Ok(Modifier::Reroll(comparison, integer)),
			Err(parse_error) => {
				self.current_index = start_index;
				Err(parse_error)
			}
		}
	}
	fn parse_successes(&mut self) -> Result<Modifier, ParseError> {
		match self.parse_require_comparison_and_require_integer() {
			Ok((comparison, integer)) => Ok(Modifier::Success(comparison, integer)),
			Err(parse_error) => Err(parse_error)
		}
	}
	fn parse_cirtical(&mut self) -> Result<Modifier, ParseError> {
		super::log("critical");
		match self.current()?.clone() {
			Lexeme::Literal(token) => match token.source() {
				"cs" | "cf" => {
					super::log("found c");
					let start_index = self.current_index;
					self.step_lexemes(1);
					super::log(&format!("current: {:?}", self.current()?));
					let (comparison, integer) = match self.parse_comparison_and_require_integer() {
						Ok((comparison, integer)) => (comparison, integer),
						Err(parse_error) => {
							super::log(&format!("parse error: {:?}", parse_error));
							self.current_index = start_index;
							return Err(parse_error);
						}
					};
					match token.source() {
						"cs" => Ok(Modifier::CriticalSuccess(comparison, integer)),
						"cf" => Ok(Modifier::CriticalFailure(comparison, integer)),
						_ => Err(ParseError::Unknown)
					}
				},
				_ => Err(ParseError::DoesNotMatch)
			},
			_ => Err(ParseError::DoesNotMatch)
		}
	}




	fn step_lexemes(&mut self, n: usize) {
		self.current_index += n;
	}

	fn current(&self) -> Result<&Lexeme, ParseError> {
		match self.current_as_option() {
			Some(lexeme) => Ok(lexeme),
			None => Err(ParseError::OutOfBounds)
		}
	}
	fn next(&self) -> Result<&Lexeme, ParseError> {
		match self.next_as_option() {
			Some(lexeme) => Ok(lexeme),
			None => Err(ParseError::OutOfBounds)
		}
	}
	fn current_as_option(&self) -> Option<&Lexeme> {
		self.lexemes.get(self.current_index)
	}
	fn next_as_option(&self) -> Option<&Lexeme> {
		self.lexemes.get(self.current_index + 1)
	}

	fn is_what_punctuation(&self, lexeme: &Lexeme, punctuation: &str) -> Result<Token, ParseError> {
		match lexeme {
			Lexeme::Punctuation(token) => if token.source() == punctuation {
				return Ok(token.clone());
			},
			_ => (),
		};
		Err(ParseError::DoesNotMatch)
	}
	fn is_what_literal(&self, lexeme: &Lexeme, literal: &str) -> Result<Token, ParseError> {
		match lexeme {
			Lexeme::Literal(token) => if token.source() == literal {
				return Ok(token.clone());
			},
			_ => (),
		};
		Err(ParseError::DoesNotMatch)
	}
	fn is_what_operator(&self, lexeme: &Lexeme, operator: &str) -> Result<Token, ParseError> {
		match lexeme {
			Lexeme::Operator(token) => if token.source() == operator {
				return Ok(token.clone());
			},
			_ => (),
		};
		Err(ParseError::DoesNotMatch)
	}

	fn is_roll(&self) -> bool {
		let current = self.current_as_option();
		let slash = current.is_some() && match current.unwrap() {
			Lexeme::Operator(token) => token.source() == "/",
			_ => false,
		};
		let next = self.next_as_option();
		let roll = next.is_some() && match next.unwrap() {
			Lexeme::Literal(token) => token.source() == "roll",
			_ => false,
		};
		slash && roll
	}
	fn is_inline_roll(&self) -> bool {
		let is_open_bracket = |lexeme: &Option<&Lexeme>| -> bool {
			lexeme.is_some() && match lexeme.unwrap() {
				Lexeme::Punctuation(token) => token.source() == "[",
				_ => false,
			}
		};
		let current = is_open_bracket(&self.current_as_option());
		let next = is_open_bracket(&self.next_as_option());
		current && next
	}
	fn is_end_of_roll(&self) -> bool {
		let is_backslash = |lexeme: &Option<&Lexeme>| -> bool {
			lexeme.is_some() && match lexeme.unwrap() {
				Lexeme::Punctuation(token) => token.source() == "\\",
				_ => false,
			}
		}; 
		let is_close_bracket = |lexeme: &Option<&Lexeme>| -> bool {
			lexeme.is_some() && match lexeme.unwrap() {
				Lexeme::Punctuation(token) => token.source() == "]",
				_ => false,
			}
		};
		match self.state {
			State::Roll(kind) => match kind {
				Roll::Explicit => is_backslash(&self.current_as_option()),
				Roll::Inline => {
					let current = is_close_bracket(&self.current_as_option());
					let next = is_close_bracket(&self.next_as_option());
					current && next
				},
			},
			_ => false,
		}
	}
}



mod ast {
	pub type Root = Vec<Node>;

	#[derive(Debug, Clone)]
	pub struct StringLiteral {
		s: String,
		previous_size: usize,
	}
	impl Default for StringLiteral {
		fn default() -> StringLiteral {
			StringLiteral {
				s: String::new(),
				previous_size: 0,
			}
		}
	}
	impl StringLiteral {
		pub fn new(s: &str) -> StringLiteral {
			StringLiteral {
				s: s.to_owned(),
				previous_size: 0,
			}
		}
		pub fn append(&mut self, s: &str) {
			self.previous_size = self.s.len();
			self.s.push_str(s);
		}
		pub fn remove_last(&mut self) {
			self.s = self.s[0..self.previous_size].to_owned();
		}
	}

	#[derive(Debug, Copy, Clone)]
	pub enum Number {
		Integer(u32),
		Float(f32),
	}
	#[derive(Debug, Copy, Clone)]
	pub enum Comparison {
		LessThan,
		GreaterThan,
		LessThanEqual,
		GreaterThanEqual,
		Equal,
	}
	#[derive(Debug, Clone)]
	pub enum Modifier {
		Exploding(Comparison, Option<u32>),
		Compounding(Comparison, Option<u32>),
		Reroll(Comparison, u32),
		KeepHighest(u32),
		KeepLowest(u32),
		DropHighest(u32),
		DropLowest(u32),
		CriticalSuccess(Comparison, u32),
		CriticalFailure(Comparison, u32),
		Success(Comparison, u32),
	}
	#[derive(Debug, Copy, Clone)]
	pub struct Normal {
		pub count: u32,
		pub sides: u32,
	}
	#[derive(Debug, Copy, Clone)]
	pub struct Fate {
		pub count: u32,
	}
	#[derive(Debug, Clone)]
	pub struct Computed {
		pub count: Box<Expression>,
		pub sides: Box<Expression>,
	}
	#[derive(Debug, Clone)]
	pub enum Dice {
		Normal(Normal, Vec<Modifier>),
		Fate(Fate, Vec<Modifier>),
		Computed(Computed, Vec<Modifier>),
	}
	#[derive(Debug, Clone)]
	pub enum Function {
		Floor(Box<Expression>),
		Ceil(Box<Expression>),
		Round(Box<Expression>),
		Abs(Box<Expression>),
	}
	#[derive(Debug, Clone)]
	pub enum Exponent {
		Function(Function),
		Dice(Dice),
		Number(Number),
		Expression(Box<Expression>),
	}
	#[derive(Debug, Clone)]
	pub enum Factor {
		Pow(Exponent, Box<Factor>),
		Exponent(Exponent),
	}
	#[derive(Debug, Clone)]
	pub enum Term {
		Multiply(Box<Term>, Factor),
		Divide(Box<Term>, Factor),
		Factor(Factor),
	}
	#[derive(Debug, Clone)]
	pub enum Expression {
		Add(Box<Expression>, Term),
		Subtract(Box<Expression>, Term),
		Term(Term),
	}
	#[derive(Debug, Clone)]
	pub enum Node {
		StringLiteral(StringLiteral),
		Expression(Expression),
		ParseError(super::ParseError),
	}
}