// File: index.js

import('./styles.css');
import('./tabs.css');
import('./result.css');
import('./grammer.css');

const rust = import('./../pkg');

const sourceInputField = document.getElementById('input-textarea');
const history = document.getElementById('history');
let historyIDCounter = 0;
let currentSelectedHistoryElement = 0;

class Tab {
	constructor(name) {
		this.name = name;
		this.tabID = 'tab-name-' + name;
		this.contentsID = name + '-section';
	}
	get tab() {
		return document.getElementById(this.tabID);
	}
	get contents() {
		return document.getElementById(this.contentsID);
	}
	clone() {
		return new Tab(this.name);
	}
	setActive(settingAs) {
		if (settingAs) {
			this.tab.classList.add('active-tab');
			this.contents.classList.add('active-tab-contents');
		} else {
			this.tab.classList.remove('active-tab');
			this.contents.classList.remove('active-tab-contents');
		}
	}
}
const historyTab = new Tab('history');
const characterSheetTab = new Tab('character-sheets');
const macrosTab = new Tab('macros');
const tablesTab = new Tab('tables');

historyTab.setActive(true);
let currentSelectedTab = historyTab;

module.exports = {
	run: function() {
		rust.then((interpreter) => {
			const source = readSource();
			sourceInputField.value = '';
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

		return false;
	},
	inputFocusIn: function() {
		sourceInputField.addEventListener('keydown', handleKeyDown);
	},
	inputFocusOut: function() {
		sourceInputField.removeEventListener('keydown', handleKeyDown);
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
	selectTab: function(tabName) {
		currentSelectedTab.setActive(false);
		if (tabName === historyTab.name) {
			historyTab.setActive(true);
			currentSelectedTab = historyTab;
		} else if (tabName === characterSheetTab.name) {
			characterSheetTab.setActive(true);
			currentSelectedTab = characterSheetTab;
		} else if (tabName === macrosTab.name) {
			macrosTab.setActive(true);
			currentSelectedTab = macrosTab;
		} else if (tabName === tablesTab.name) {
			tablesTab.setActive(true);
			currentSelectedTab = tablesTab;
		}
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
	console.log(i, history.children, historyEntry);
	if (historyEntry === undefined) {
		return undefined;
	}
	return historyEntry.querySelector(".source").innerHTML;
}
function handleKeyDown(event) {
	if (event.keyCode === 13) { // enter
		module.exports.run();
		event.preventDefault();
	} else if (event.keyCode === 38) { // up
		let source = getSourceFromHistory(currentSelectedHistoryElement-1);
		console.log(history, source);
		if (source === undefined) {
			sourceInputField.value = '';
			currentSelectedHistoryElement = history.childElementCount;
		} else {
			currentSelectedHistoryElement -= 1;
			sourceInputField.value = source;
		}
	} else if (event.keyCode == '40') { // down
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