// File: parser/state.rs

#[derive(Debug, Copy, Clone)]
pub(super) enum State {
	Start,
	StringLiteral,
	Roll,
	Done,
}
impl Default for State {
	fn default() -> State {
		State::Start
	}
}
