// slider.js

const sliderBarID = 'slider-bar';
const sliderBar = document.getElementById(sliderBarID);
const gridContainer = document.getElementById('page-container');
let handlingDragging = false;

function StartDrag(event) {
	handlingDragging = true;
	gridContainer.style.cursor = 'ew-resize';
}
function onDrag(event) {
	if (handlingDragging) {
		gridContainer.style.gridTemplateColumns = `1fr 6px ${window.innerWidth-event.clientX-3}px`;
	}
}
function endDrag(event) {
	handlingDragging = false;
	gridContainer.style.cursor = 'auto';
}

sliderBar.addEventListener('mousedown', StartDrag);
gridContainer.addEventListener('mousemove', onDrag);
gridContainer.addEventListener('mouseup', endDrag);
