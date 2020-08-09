// File: index.js

import('./styles.css');

const rust = import('./pkg');

const sourceInputField = document.getElementById('source');
const history = document.getElementById('history');
const saveHistoryName = document.getElementById('save history name');

let historyIDCounter = 0;

module.exports = {
	run: function() {
		rust
			.then((interpreter) => {
				let t0 = performance.now();
				const source = readSource();
				if (source.length > 0) {
					const result = interpreter.run(source);
					appendHistory(source, result);
				}
				let t1 = performance.now();
				console.log(`Call to interpreter.run() took ${t1 - t0} milliseconds.`);
			})
			.catch(console.error);

		return false;
	},
	clearHistory: function() {
		while (history.firstChild) {
			history.removeChild(history.lastChild);
		}
	},
	saveHistory: function() {

	},
	deleteHistoryEntry: function(id) {
		let entry = document.getElementById(id);
		history.removeChild(entry);
	}
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
	resultElement.innerHTML = result;
	deleteEntry.type = 'button';
	deleteEntry.innerHTML = 'Delete Entry';
	deleteEntry.setAttribute('onclick', `RollLang.deleteHistoryEntry(${historyIDCounter})`);
	historyIDCounter += 1;

	latestEntry.appendChild(sourceElement);
	latestEntry.appendChild(resultElement);
	latestEntry.appendChild(deleteEntry);
	history.appendChild(latestEntry);
}