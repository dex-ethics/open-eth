'use strict';

// Show a <form> as a modal or fullscreen dialogue
// TODO: Make the browser back button act as [cancel]
function show_form(fullscreen, form, on_accept, on_reject)
{
	if(form.constructor == String)
		form = document.getElementById(form);
	let body = document.getElementsByTagName('body')[0];
	let backdrop = document.getElementById('modal_backdrop');
	if(backdrop === null) {
		backdrop = document.createElement('div');
		backdrop.id = 'modal_backdrop';
		backdrop.setAttribute('hidden','');
		body.appendChild(backdrop);
	}
	if(fullscreen) {
		form.className += ' fullscreen_dialog';
	} else {
		form.className += ' modal_dialog';
		backdrop.removeAttribute('hidden');
	}
	form.removeAttribute('hidden');
	body.style.overflow = 'hidden';
	
	let backdrop_click = function(event) {
		form.reset();
		event.preventDefault();
		event.stopPropagation();
	};
	let escape_handler = function(event) {
		if(event.keyCode === 27) {
			form.reset();
			event.preventDefault();
			event.stopPropagation();
		}
	};
	let form_reset;
	let form_submit;
	let close = function(accept, event) {
		// Get the value of the submit button used
		let value = undefined;
		if(document.activeElement instanceof HTMLButtonElement
			&& document.activeElement.type === "submit") {
			value = document.activeElement.value;
		}
		
		if(fullscreen) {
			form.className = form.className.replace(/\bfullscreen_dialog\b/,'');
		} else {
			form.className = form.className.replace(/\bmodal_dialog\b/,'');
			backdrop.setAttribute('hidden','');
			backdrop.removeEventListener('click', backdrop_click, true);
		}
		form.setAttribute('hidden','');
		body.style.overflow = '';
		document.removeEventListener('keydown', escape_handler, true);
		form.removeEventListener('reset', form_reset, true);
		form.removeEventListener('submit', form_submit, true);
		event.preventDefault();
		event.stopPropagation();
		
		if(accept && typeof on_accept === 'function')
			on_accept(value);
		if(!accept && typeof on_reject === 'function')
			on_reject();
	};
	form_reset = close.bind(null, false);
	form_submit = close.bind(null, true);
	if(!fullscreen) {
		backdrop.addEventListener('click', backdrop_click, true);
	}
	document.addEventListener('keydown', escape_handler, true);
	form.addEventListener('reset', form_reset, true);
	form.addEventListener('submit', form_submit, true);
}
let show_modal = show_form.bind(null, false);
let show_fullscreen = show_form.bind(null, true);
