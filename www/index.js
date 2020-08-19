// File: index.js

import('./js/slider.js');
const Tabs = require('./js/tabs.js').Tabs;
let tabs = new Tabs();

// import('./css/styles.css');
// import('./css/tabs.css');

// import('./css/history.css');
// import('./css/result.css');

// import('./css/character-sheet.css');

// import('./css/macros.css');

// import('./css/tables.css');

// import('./css/guide.css');
// import('./css/grammer.css');

const rust = import('./../pkg');


module.exports = {
	init: function() {
		rust.then((interpreter) => {
			interpreter.init();
		})
	},

	run: function() {
		const source = readSource();
		sourceInputField.value = '';

		runSource(source);

		return false;
	},

	selectTab: function(tabName) {
		tabs.selectTab(tabName);
	},

	clearHistory: function() {
		while (history.firstChild) {
			history.removeChild(history.lastChild);
		}
		historySources = new Map();
	},
	deleteHistoryEntry: function(id) {
		let entry = document.getElementById(id);
		history.removeChild(entry);
	},
	inputFocusIn: function() {
		sourceInputField.addEventListener('keydown', handleKeyDown);
	},
	inputFocusOut: function() {
		sourceInputField.removeEventListener('keydown', handleKeyDown);
	},


	handleMacroUpdateCreate: function() {
		rust.then((interpreter) => {
			interpreter.handle_macro_update_create();
		})
		.catch(console.error);
	},
	handleMacroDelete: function(name) {
		rust.then((interpreter) => {
			interpreter.handle_macro_delete(name);
		})
		.catch(console.error);
	},

	handleMacroSelect: function(name) {
		rust.then((interpreter) => {
			interpreter.handle_macro_select(name);
		})
		.catch(console.error);
	},
	handleMacroChangeInBar: function(name) {
		rust.then((interpreter) => {
			interpreter.handle_macro_change_in_bar(name);
		})
		.catch(console.error);
	},

	handleMacroTest: function() {
		const macroSource = macroSourceElement.value;
		runSource(macroSource);
	},
	runMacro: function(name) {
		rust.then((interpreter) => {
			const source = interpreter.macro_source(name);
			runSource(source);
		})
		.catch(console.error);
	},
}

const macroNameElement = document.getElementById('create-macro-name');
const macroSourceElement = document.getElementById('create-macro-text');

function runSource(source) {
	rust.then((interpreter) => {
		if (source.length > 0) {
			let t0 = performance.now();
			const result = interpreter.run(source);
			let t1 = performance.now();
			console.log(`Call to interpreter.run() took ${t1 - t0} milliseconds.`);
			appendHistory(source, result);
			currentSelectedHistoryElement = history.childElementCount;
		}
	})
	.catch(console.error);
}


function readSource() {
	return sourceInputField.value;
}
function appendHistory(source, result) {
	const latestEntry = document.createElement('li');
	const sourceElement = document.createElement('p');
	const resultElement = document.createElement('p');
	const deleteEntry = document.createElement('button');

	latestEntry.setAttribute('id', historyIDCounter);
	sourceElement.innerHTML = source;
	sourceElement.setAttribute('class', 'source');
	resultElement.innerHTML = result;
	deleteEntry.type = 'button';
	deleteEntry.innerHTML = 'Delete Entry';
	deleteEntry.setAttribute('onclick', `RollLang.deleteHistoryEntry(${historyIDCounter})`);
	historyIDCounter += 1;

	latestEntry.appendChild(sourceElement);
	latestEntry.appendChild(resultElement);
	latestEntry.appendChild(deleteEntry);
	history.appendChild(latestEntry);
	history.scrollTop = 100000;
}
function getSourceFromHistory(i) {
	const historyEntry = history.children[i];
	if (historyEntry === undefined) {
		return undefined;
	}
	return historyEntry.querySelector(".source").innerHTML;
}
function handleKeyDown(event) {
	if (event.keyCode === 13) { // enter
		event.preventDefault();
		module.exports.run();
	} else if (event.keyCode === 38) { // up
		event.preventDefault();
		if (currentSelectedHistoryElement < 1) {
			return;
		}

		let source = getSourceFromHistory(currentSelectedHistoryElement-1);
		if (source === undefined) {
			sourceInputField.value = '';
			currentSelectedHistoryElement = history.childElementCount;
		} else {
			currentSelectedHistoryElement -= 1;
			sourceInputField.value = source;
		}
	} else if (event.keyCode == '40') { // down
		event.preventDefault();
		let source = getSourceFromHistory(currentSelectedHistoryElement+1);
		if (source === undefined) {
			sourceInputField.value = '';
			currentSelectedHistoryElement = history.childElementCount;
		} else {
			currentSelectedHistoryElement += 1;
			sourceInputField.value = source;
		}
	}
}






const sourceInputField = document.getElementById('input-textarea');
const history = document.getElementById('history');
let historyIDCounter = 0;
let currentSelectedHistoryElement = 0;

