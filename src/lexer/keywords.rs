// File: src/lexer/keywords.rs

pub static KEYWORDS: [&str; 35] = [
	// ambiguous
	"r",
	"d",
	// init
	"roll",
	// functions
	"abs",
	"ceil",
	"floor",
	"round",
	"round_half_down",
	// dice
	"dF",
	// modifiers
	// drops/keep
	"drop",
	"dh",
	"dl",
	"k",
	"keep",
	"kh",
	"kl",
	"highest",
	"lowest",
	// rerolls
	"reroll",
	"ro",
	"once",
	// expanding
	"exploding",
	"compounding",
	"penetrating",
	// success/fails
	"cs",
	"cf",
	"critical",
	"success",
	"failure",
	// sorts
	"s",
	"sort",
	"sa",
	"sd",
	"ascending",
	"decending",
];
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Keyword {
	// ambiguous
	R,
	D,
	// init
	Roll,
	// functions
	Abs,
	Ceil,
	Floor,
	Round,
	RoundHalfDown,
	// dice
	FateDice,
	// modifiers
	// drop
	Drop,
	DropHighest,
	DropLowest,
	// keep
	Keep,
	KeepHighest,
	KeepLowest,
	Lowest,
	Highest,
	// rerolls
	Reroll,
	RerollOnce,
	Once,
	// expanding
	Exploding,
	Compounding,
	Penetrating,
	// success/fail
	CriticalSuccess,
	CriticalFailure,
	Critical,
	Success,
	Failure,
	// sort
	Sort,
	SortAscending,
	SortDecending,
	Ascending,
	Decending,
}
impl std::convert::TryFrom<&str> for Keyword {
	type Error = ();
	fn try_from(word: &str) -> Result<Keyword, Self::Error> {
		match word {
			"d" => Ok(Self::D),
			"r" => Ok(Self::R),
			"roll" => Ok(Self::Roll),
			"abs" => Ok(Self::Abs),
			"ceil" => Ok(Self::Ceil),
			"floor" => Ok(Self::Floor),
			"round" => Ok(Self::Round),
			"round_half_down" => Ok(Self::RoundHalfDown),
			"dF" => Ok(Self::FateDice),
			"drop" => Ok(Self::Drop),
			"keep" => Ok(Self::Keep),
			"dh" => Ok(Self::DropHighest),
			"dl" => Ok(Self::DropLowest),
			"k" | "kh" => Ok(Self::KeepHighest),
			"kl" => Ok(Self::KeepLowest),
			"lowest" => Ok(Self::Lowest),
			"highest" => Ok(Self::Highest),
			"reroll" => Ok(Self::Reroll),
			"ro" => Ok(Self::RerollOnce),
			"once" => Ok(Self::Once),
			"exploding" => Ok(Self::Exploding),
			"compounding" => Ok(Self::Compounding),
			"penetrating" => Ok(Self::Penetrating),
			"cs" => Ok(Self::CriticalSuccess),
			"cf" => Ok(Self::CriticalFailure),
			"critical" => Ok(Self::Critical),
			"success" => Ok(Self::Success),
			"failure" => Ok(Self::Failure),
			"sort" => Ok(Self::Sort),
			"s" | "sa" => Ok(Self::SortAscending),
			"sd" => Ok(Self::SortDecending),
			"ascending" => Ok(Self::Ascending),
			"decending" => Ok(Self::Decending),
			_ => Err(()),
		}
	}
}
