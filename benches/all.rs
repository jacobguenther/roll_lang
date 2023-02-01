// File: benches/all.rs

use criterion::{black_box, criterion_group, criterion_main, Criterion};

use std::iter::Iterator;

const ADDITION: &'static str = "/roll 1+2+3+4+5";
const DICE: &'static str = "/roll d20 + 4/2[modifier] + [[4d6]][inline]";
const MULTIPLE_ROLLS: &'static str = "/r d20 /r d6 /r d3 \\ [[4]] [[5]] [[6]]";

pub fn bench_lexer(c: &mut Criterion) {
	use roll_lang::lexer::keywords::*;
	use roll_lang::lexer::*;

	let digits = "123456 7890123 4567890 123456 789012 345678 90123 45678 9012 3456 789 0";
	c.bench_function("lexer digits", |b| {
		b.iter(|| {
			let _ = Lexer::new(black_box(digits)).collect::<Vec<_>>();
		})
	});

	let keywords = KEYWORDS.iter().cloned().collect::<String>();
	let many_keywords = format!(
		"{}{}{}{}{}{}{}{}",
		keywords, keywords, keywords, keywords, keywords, keywords, keywords, keywords
	);
	c.bench_function("lexer keywords", |b| {
		b.iter(|| {
			let _ = Lexer::new(black_box(many_keywords.as_str())).collect::<Vec<_>>();
		})
	});

	c.bench_function("lexer addition", |b| {
		b.iter(|| {
			let _ = Lexer::new(black_box(ADDITION)).collect::<Vec<_>>();
		})
	});
	c.bench_function("lexer dice", |b| {
		b.iter(|| {
			let _ = Lexer::new(black_box(DICE)).collect::<Vec<_>>();
		})
	});
}

pub fn bench_parser(c: &mut Criterion) {
	use roll_lang::parser::*;

	c.bench_function("parser addition", |b| {
		b.iter(|| {
			let _ast = Parser::new(black_box(ADDITION)).parse();
		})
	});

	c.bench_function("parser dice", |b| {
		b.iter(|| {
			let _ast = Parser::new(black_box(DICE)).parse();
		})
	});

	c.bench_function("parser multiple rolls", |b| {
		b.iter(|| {
			let _ast = Parser::new(black_box(MULTIPLE_ROLLS)).parse();
		})
	});
}

#[cfg(feature = "use-rand")]
pub fn bench_interpreter(c: &mut Criterion) {
	use roll_lang::builder::*;
	use roll_lang::interpreter::*;

	c.bench_function("interpreter addition", |b| {
		b.iter(|| {
			InterpreterBuilder::default()
				.build(rand::random::<f64>)
				.interpret(black_box(ADDITION));
		})
	});

	c.bench_function("interpreter dice", |b| {
		b.iter(|| {
			InterpreterBuilder::default()
				.build(rand::random::<f64>)
				.interpret(black_box(DICE));
		})
	});

	c.bench_function("interpreter multiple rolls", |b| {
		b.iter(|| {
			InterpreterBuilder::default()
				.build(rand::random::<f64>)
				.interpret(black_box(MULTIPLE_ROLLS));
		})
	});
}

#[cfg(feature = "use-rand")]
criterion_group!(benches, bench_lexer, bench_parser, bench_interpreter);
#[cfg(not(feature = "use-rand"))]
criterion_group!(benches, bench_lexer, bench_parser);

criterion_main!(benches);
