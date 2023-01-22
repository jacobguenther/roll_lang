// File: benches/all.rs

use criterion::{black_box, criterion_group, criterion_main, Criterion};

use roll_lang::lexer::keywords::*;
use roll_lang::lexer::*;
use std::iter::Iterator;

pub fn bench_lexer(c: &mut Criterion) {
	let digits = "123456789012345678901234567890123456789012345678901234567890";
	c.bench_function("lexer digits", |b| {
		b.iter(|| {
			black_box(Lexer::new(digits).collect::<Vec<_>>());
		})
	});

	let keywords = KEYWORDS.iter().cloned().collect::<String>();
	let many_keywords = format!(
		"{}{}{}{}{}",
		keywords, keywords, keywords, keywords, keywords
	);
	c.bench_function("lexer keywords", |b| {
		b.iter(|| {
			black_box(Lexer::new(many_keywords.as_str()).collect::<Vec<_>>());
		})
	});
}

use roll_lang::interpreter::*;
use roll_lang::builder::*;

pub fn bench_interpreter(c: &mut Criterion) {
	let addition = "/roll 1+2+3+4+5";
	c.bench_function("interpreter addition", |b| {
		b.iter(|| {
			InterpreterBuilder::default().build(rand::random::<f64>).interpret(black_box(addition));
		})
	});

	let dice = "/roll d20 + 4/2[modifier] + [[4d6]][inline]";
	c.bench_function("interpreter dice", |b| {
		b.iter(|| {
			InterpreterBuilder::default().build(rand::random::<f64>).interpret(black_box(dice));
		})
	});
}

criterion_group!(benches, bench_lexer, bench_interpreter);
criterion_main!(benches);