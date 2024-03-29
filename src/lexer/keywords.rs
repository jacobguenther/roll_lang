// File: src/lexer/keywords.rs

use std::convert::TryFrom;

// must be alphebetical
pub static KEYWORDS: [&str; 35] = [
	"abs",
	"ascending",
	"ceil",
	"cf",
	"compounding",
	"critical",
	"cs",
	"d",
	"descending",
	"dF",
	"dh",
	"dl",
	"drop",
	"exploding",
	"failure",
	"floor",
	"highest",
	"k",
	"keep",
	"kh",
	"kl",
	"lowest",
	"once",
	"penetrating",
	"r",
	"reroll",
	"ro",
	"roll",
	"round",
	"round_half_down",
	"s",
	"sa",
	"sd",
	"sort",
	"success",
];
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Keyword {
	Abs,
	Ascending,
	Ceil,
	CriticalFailure,
	Compounding,
	Critical,
	CriticalSuccess,
	D,
	DF,
	Descending,
	DropHighest,
	DropLowest,
	Drop,
	Exploding,
	Failure,
	Floor,
	Highest,
	Keep,
	KeepHighest,
	KeepLowest,
	Lowest,
	Once,
	Penetrating,
	R,
	Reroll,
	RerollOnce,
	Roll,
	Round,
	RoundHalfDown,
	SortAscending,
	SortDescending,
	Sort,
	Success,

	NotFound,
}
impl TryFrom<&str> for Keyword {
	type Error = ();
	fn try_from(word: &str) -> Result<Keyword, Self::Error> {
		match word {
			"abs" => Ok(Self::Abs),
			"ascending" => Ok(Self::Ascending),
			"ceil" => Ok(Self::Ceil),
			"cf" => Ok(Self::CriticalFailure),
			"compounding" => Ok(Self::Compounding),
			"critical" => Ok(Self::Critical),
			"cs" => Ok(Self::CriticalSuccess),
			"d" => Ok(Self::D),
			"dF" => Ok(Self::DF),
			"descending" => Ok(Self::Descending),
			"dh" => Ok(Self::DropHighest),
			"dl" => Ok(Self::DropLowest),
			"drop" => Ok(Self::Drop),
			"exploding" => Ok(Self::Exploding),
			"failure" => Ok(Self::Failure),
			"floor" => Ok(Self::Floor),
			"highest" => Ok(Self::Highest),
			"k" => Ok(Self::KeepHighest),
			"keep" => Ok(Self::Keep),
			"kh" => Ok(Self::KeepHighest),
			"kl" => Ok(Self::KeepLowest),
			"lowest" => Ok(Self::Lowest),
			"once" => Ok(Self::Once),
			"penetrating" => Ok(Self::Penetrating),
			"r" => Ok(Self::R),
			"reroll" => Ok(Self::Reroll),
			"ro" => Ok(Self::RerollOnce),
			"roll" => Ok(Self::Roll),
			"round" => Ok(Self::Round),
			"round_half_down" => Ok(Self::RoundHalfDown),
			"s" | "sa" => Ok(Self::SortAscending),
			"sd" => Ok(Self::SortDescending),
			"sort" => Ok(Self::Sort),
			"success" => Ok(Self::Success),
			_ => Err(()),
		}
	}
}
