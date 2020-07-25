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
	// fn parse_binary_op(&mut self) -> Result<BinaryOp, ParseError>;
	// fn parse_unary_op(&mut self) -> Result<UnaryOp, ParseError>;
	fn parse_dice(&mut self) -> Result<Dice, ParseError>;
	fn parse_dice_letter(&mut self, count: u32) -> Result<Dice, ParseError>;
	fn parse_dice_sides(&mut self, count: u32) -> Result<Dice, ParseError>;

	fn parse_number(&mut self) -> Result<Number, ParseError>;
	fn parse_float(&mut self, whole_number: &Token) -> Result<Number, ParseError>;

	fn step_lexemes(&mut self, n: usize);
	fn step_back(&mut self, n: usize);

	fn current(&self) -> Option<&Lexeme>;
	fn next(&self) -> Option<&Lexeme>;

	fn is_roll(&self) -> bool;
	fn is_inline_roll(&self) -> bool;
	fn is_end_of_roll(&self) -> bool;
}

#[derive(Debug, Clone)]
pub enum ParseError {
	UnexpectedToken(Token),
	DoesNotMatch,
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
		} else if self.current().is_none() {
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
			} else if self.current().is_none() {
				self.state = State::Done;
				break;
			}
		}
		literal
	}

	fn parse_expression(&mut self) -> Result<Expression, ParseError> {
		match self.current().unwrap() {
			Lexeme::Whitespace(_token) => self.step_lexemes(1),
			_=> (),
		}

		self.state = State::Done;

		// 	// Lexeme::Literal(token) => match token.source() {
		// 	// 	"abs" => Err(ParseError::UnexpectedToken(self.current().token().clone())),
		// 	// 	"floor" => Err(ParseError::UnexpectedToken(self.current().token().clone())),
		// 	// 	"ceil" => Err(ParseError::UnexpectedToken(self.current().token().clone())),
		// 	// 	_ => Err(ParseError::UnexpectedToken(self.current().token().clone())),
		// 	// },

		match self.parse_dice() {
			Ok(dice) => return Ok(Expression::Dice(dice)),
			Err(_) => (),
		};
		match self.parse_number() {
		 	Ok(num) => return Ok(Expression::Number(num)),
		 	Err(_)  => (),
		};

		Err(ParseError::Unknown)
	}
	// fn parse_unary_op(&mut self) -> Result<UnaryOp, ParseError> {
	// 	todo!();
	// }
	// fn parse_binary_op(&mut self) -> Result<BinaryOp, ParseError> {
	// 	todo!();
	// }
	fn parse_dice(&mut self) -> Result<Dice, ParseError> {
		let count = self.parse_number();
		match count {
			Ok(num) => match num {
				Number::Integer(i) => self.parse_dice_letter(i),
				Number::Float(_) => {
					self.step_back(3);
					Err(ParseError::DoesNotMatch)
				},
			},
			Err(e) => Err(e),
		}
	}
	fn parse_dice_letter(&mut self, count: u32) -> Result<Dice, ParseError> {
		match self.current().unwrap().clone() {
			Lexeme::Literal(token) => {
				if token.source().to_lowercase() == "d" {
					match self.parse_dice_sides(count) {
						Ok(dice) => Ok(dice),
						Err(e) => {
							Err(e)
						},
					}
				} else {
					Err(ParseError::DoesNotMatch)
				}
			}
			_ => Err(ParseError::DoesNotMatch),
		}
	}
	fn parse_dice_sides(&mut self, count: u32) -> Result<Dice, ParseError> {
		let sides = self.parse_number();
		match sides {
			Ok(num) => match num {
				Number::Integer(i) => Ok(Dice {count: count, sides: i}),
				Number::Float(_) => Err(ParseError::DoesNotMatch)
			},
			Err(e) => Err(e),
		}
	}
	fn parse_number(&mut self) -> Result<Number, ParseError> {
		match self.current().unwrap().clone() {
			Lexeme::Number(token) => {
				self.step_lexemes(1);
				match self.parse_float(&token) {
					Ok(float) => {
						Ok(float)
					},
					Err(_) => {
						Ok(Number::Integer(token.source().parse::<u32>().unwrap()))
					},
				}
			},
			_ => Err(ParseError::DoesNotMatch)
		}
	}
	fn parse_float(&mut self, whole_number: &Token) -> Result<Number, ParseError> {
		let is_period = |lexeme: &Option<&Lexeme>| -> bool {
			lexeme.is_some() && match lexeme.unwrap() {
				Lexeme::Punctuation(token) => token.source() == ".",
				_ => false,
			}
		};
		let is_number = |lexeme: &Option<&Lexeme>| -> Option<Token> {
			if lexeme.is_some() {
				match lexeme.unwrap() {
					Lexeme::Number(token) => return Some(token.clone()),
					_ => ()
				}
			}
			None
		};

		let decimal_token = is_number(&self.next());
		if is_period(&self.current()) && decimal_token.is_some() {
			self.step_lexemes(2);
			let float = format!("{}.{}", whole_number.source(), decimal_token.unwrap().source())
				.parse::<f32>().unwrap();
			Ok(Number::Float(float))
		} else {
			Err(ParseError::DoesNotMatch)
		}
	}

	fn step_lexemes(&mut self, n: usize) {
		self.current_index += n;
	}
	fn step_back(&mut self, n: usize) {
		self.current_index -= n;
	}

	fn current(&self) -> Option<&Lexeme> {
		self.lexemes.get(self.current_index)
	}
	fn next(&self) -> Option<&Lexeme> {
		self.lexemes.get(self.current_index + 1)
	}

	fn is_roll(&self) -> bool {
		let slash = self.current().is_some() && match self.current().unwrap() {
			Lexeme::Operator(token) => token.source() == "/",
			_ => false,
		};
		let roll = self.next().is_some() && match self.next().unwrap() {
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
		let current = is_open_bracket(&self.current());
		let next = is_open_bracket(&self.next());
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
				Roll::Explicit => is_backslash(&self.current()),
				Roll::Inline => {
					let current = is_close_bracket(&self.current());
					let next = is_close_bracket(&self.next());
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
	impl Number {
		fn is_number(&self) -> bool {
			match self {
				Number::Integer(_) => true,
				Number::Float(_) => false,
			}
		}
	}
	#[derive(Debug, Copy, Clone)]
	pub struct Dice {
		pub count: u32,
		pub sides: u32,
	}
	#[derive(Debug, Copy, Clone)]
	pub enum ExplodingDiceComparison {
		LessThan,
		GreaterThan,
		LessThanEqual,
		GreaterThanEqual,
		Equal,
	}
	impl Default for ExplodingDiceComparison {
		fn default() -> ExplodingDiceComparison {
			ExplodingDiceComparison::Equal
		}
	}
	#[derive(Debug, Clone)]
	pub struct ExplodingDiceTerms {
		d: Dice,
		comparison: ExplodingDiceComparison,
		check: Box<Expression>,
	}
	#[derive(Debug, Copy, Clone)]
	pub enum LowHigh {
		Low,
		High,
	}
	#[derive(Debug, Clone)]
	pub struct DropKeepTerms {
		d: Dice,
		low_high: LowHigh,
		count: Box<Expression>,
	}
	#[derive(Debug, Clone)]
	pub struct BinaryOpTerms {
		lhs: Box<Expression>,
		rhs: Box<Expression>,
	}
	#[derive(Debug, Clone)]
	pub enum Comparison {
		LessThan(BinaryOpTerms),
		GreaterThan(BinaryOpTerms),
		LessThanEqual(BinaryOpTerms),
		GreaterThanEqual(BinaryOpTerms),
		Equal(BinaryOpTerms),
	}
	#[derive(Debug, Clone)]
	pub enum BinaryOp {
		Add(BinaryOpTerms),
		Subtract(BinaryOpTerms),
		Multiply(BinaryOpTerms),
		Divide(BinaryOpTerms),
		Modulus(BinaryOpTerms),

		ExpolingDice(ExplodingDiceTerms),
		CompoundingExplodingDice(ExplodingDiceTerms),
		DropDice(),
		KeepDice(),
	}
	#[derive(Debug, Clone)]
	pub enum UnaryOp {
		Abs(Box<Expression>),
		Floor(Box<Expression>),
		Ceil(Box<Expression>),
	}
	#[derive(Debug, Clone)]
	pub enum Expression {
		Number(Number),
		Dice(Dice),
		BinaryOp(BinaryOp),
		UnaryOp(UnaryOp),
		N
	}
	#[derive(Debug, Clone)]
	pub enum Node {
		StringLiteral(StringLiteral),
		Expression(Expression),
		ParseError(super::ParseError),
	}
}