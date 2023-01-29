// File: www/main.js

var state_handler;

function shallow_copy(object) {
	return Object.assign({}, object);
}
function deep_copy(object) {
	return JSON.parse(JSON.stringify(object));
}

function get_input_el() {
	return document.getElementById("input");
}
function get_input() {
	return get_input_el().value;
}
function get_result_input(i) {
	let results_div = document.getElementById("results_div");
	console.log(results_div.children(i));
}

class ResultsState {
	constructor() {
		this.count = 0;
		this.cursor_pos = 0;
	}
}
class InputState {
	constructor() {
		this.cursor_pos = 6;
		this.input_text = get_input();
		this.mode = "brackets"; // alt: cursor
	}
}
class State {
	constructor() {
		this.input_state = new InputState();
		this.results_state = new ResultsState();
	}
}
class StateHandler {
	constructor() {
		this.pervious_states = [];
		this.current_state = new State();
		this.future_states = [];
	}
	undo() {
		console.log("undo");
		if (this.pervious_states.length != 0) {
			this.future_states.unshift(this.current_state);
			this.current_state = this.pervious_states.pop();
		}
		document.getElementById("input_undo").disabled = false;
		document.getElementById("input_redo").disabled = false;
		if (this.pervious_states.length === 0) {
			document.getElementById("input_undo").disabled = true;
		}

		document.getElementById("input").value = this.current_state.input_state.input_text;

		return this.current_state;
	}
	redo() {
		console.log("redo");
		if (this.future_states.length != 0) {
			this.pervious_states.push(this.current_state);
			this.current_state = this.future_states.shift();
		}
		document.getElementById("input_undo").disabled = false;
		document.getElementById("input_redo").disabled = false;
		if (this.future_states.length === 0) {
			document.getElementById("input_redo").disabled = true;
		}

		document.getElementById("input").value = this.current_state.input_state.input_text;

		return this.current_state;
	}
	execute_command(command) {
		console.log("command: ", command);
		let next_state = command(this.current_state);
		this.pervious_states.push(this.current_state);
		this.future_states = [];

		document.getElementById("input").value = next_state.input_state.input_text;
		document.getElementById("input_undo").disabled = false;
		document.getElementById("input_redo").disabled = true;

		this.current_state = next_state;
	}
}

function handle_run() {
	state_handler.execute_command(function(current_state) {
		let new_state = deep_copy(current_state);
		const input = get_input();
		if (input) {
			const result = wasm_bindgen.interpret(input);
			let results_div = document.getElementById("results_div");
			results_div.innerHTML += result;

			new_state.results_state.count += 1;
			new_state.results_state.cursor_pos = new_state.results_state.count - 1;

			new_state.input_state.input_text = get_input();

			document.getElementById("results_up").disabled = false;
			document.getElementById("results_clear").disabled = false;
		}
		return new_state;
	});
}

function handle_input_undo() {
	state_handler.undo();
}
function handle_input_redo() {
	state_handler.redo();
}
function handle_input_clear() {
	state_handler.execute_command(function(current_state) {
		let new_state = deep_copy(current_state);
		new_state.input_state.input_text = "/roll [] \\";
		new_state.input_state.cursor_pos = 6;
		return new_state;
	});
}

function handle_results_up() {
	state_handler.execute_command(function(current_state) {
		let new_state = deep_copy(current_state);
		let results_state = new_state.results_state;

		document.getElementById("results_down").disabled = false;

		let source = document
			.getElementById("results_div")
			.children[results_state.cursor_pos]
			.getElementsByClassName("source")
			[0]
			.innerHTML;

		if (results_state.cursor_pos > 0) {
			results_state.cursor_pos -= 1;
		} else if (results_state.cursor_pos == 0) {
			document.getElementById("results_up").disabled = true;
		}

		new_state.input_state.input_text = source;
		return new_state;
	});
}
function handle_results_down() {
	state_handler.execute_command(function(current_state) {
		let new_state = deep_copy(current_state);
		let results_state = new_state.results_state;

		document.getElementById("results_up").disabled = false;

		results_state.cursor_pos += 1;
		let source;
		if (results_state.cursor_pos < results_state.count) {
			source = document
				.getElementById("results_div")
				.children[results_state.cursor_pos]
				.getElementsByClassName("source")
				[0]
				.innerHTML;
		} else if (results_state.cursor_pos === results_state.count) {
			document.getElementById("results_down").disabled = true;
			results_state.cursor_pos = results_state.count - 1;
			source = "";
		}
		new_state.input_state.input_text = source;

		return new_state;
	});
}
function handle_results_clear() {
	state_handler.execute_command(function(current_state) {
		let new_state = deep_copy(current_state);
		new_state.results_state.count = 0;
		new_state.results_state.cursor_pos = 0;
		document.getElementById("results_up").disabled = true;
		document.getElementById("results_down").disabled = true;
		document.getElementById("results_clear").disabled = true;

		let results_div = document.getElementById("results_div");
		results_div.innerHTML = "";
		return new_state;
	});
}

function handle_insert(s, maybe_offset) {
	state_handler.execute_command(function(current_state) {
		let new_state = deep_copy(current_state);
		let input_state = new_state.input_state;

		let offset;
		if (typeof(maybe_offset) === 'number') {
			offset = maybe_offset;
		} else {
			offset = 0;
		}

		let part_a = input_state.input_text.slice(0, input_state.cursor_pos);
		let part_b = input_state.input_text.slice(input_state.cursor_pos+2);

		input_state.input_text = part_a + s + part_b;
		input_state.cursor_pos += offset;

		return new_state;
	});
}

async function start() {
	await wasm_bindgen('./roll_lang_bg.wasm');

	document.getElementById("input_undo").disabled = true;
	document.getElementById("input_redo").disabled = true;
	document.getElementById("results_up").disabled = true;
	document.getElementById("results_down").disabled = true;
	document.getElementById("results_clear").disabled = true;

	state_handler = new StateHandler();

	document.getElementById("run_input").onclick = handle_run;
	document.getElementById("input").onkeydown = function(event) {
		if (event.key === 'Enter' || event.key === 'enter') {
			handle_run();
		}
	};

	document.getElementById("input_undo").onclick = handle_input_undo;
	document.getElementById("input_redo").onclick = handle_input_redo;
	document.getElementById("input_clear").onclick = handle_input_clear;

	document.getElementById("results_up").onclick = handle_results_up;
	document.getElementById("results_down").onclick = handle_results_down;
	document.getElementById("results_clear").onclick = handle_results_clear;

	document.getElementById("operator_plus_button").onclick = function() {
		handle_insert("[] + []");
	};
	document.getElementById("operator_minus_button").onclick = function() {
		handle_insert("[] - []");
	};
	document.getElementById("operator_mul_button").onclick = function() {
		handle_insert("[] * []");
	};
	document.getElementById("operator_div_button").onclick = function() {
		handle_insert("[] / []");
	};
	document.getElementById("operator_power_button").onclick = function() {
		handle_insert("[] ^ ([])");
	};
	document.getElementById("operator_power_alt_button").onclick = function() {
		handle_insert("[] ** ([])");
	};
	document.getElementById("operator_unary_minus_button").onclick = function() {
		handle_insert("-[]", 1);
	};
	document.getElementById("operator_parenthese_1_button").onclick = function() {
		handle_insert("([])", 1);
	};
	document.getElementById("operator_parenthese_2_button").onclick = function() {
		handle_insert("[]([])");
	};
	document.getElementById("operator_parenthese_3_button").onclick = function() {
		handle_insert("([])([])", 1);
	};

	document.getElementById("function_floor_button").onclick = function() {
		handle_insert("floor([])", 6);
	};
	document.getElementById("function_ceil_button").onclick = function() {
		handle_insert("ceil([])", 5);
	};
	document.getElementById("function_round_button").onclick = function() {
		handle_insert("round([])", 6);
	};
	document.getElementById("function_round_half_down_button").onclick = function() {
		handle_insert("round_half_down([])", 16);
	};
	document.getElementById("function_abs_button").onclick = function() {
		handle_insert("abs([])", 4);
	};
}
