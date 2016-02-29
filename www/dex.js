'use strict';
// Author: Remco Bloemen

RegExp.escape = function(str) {
	return String(str).replace(/([-()\[\]{}+?*.$\^|,:#<!\\])/g, "\\$1");
};

let HTMLEscapeElement = document.createElement('div');
function HTMLEscape(str) {
	HTMLEscapeElement.textContent = str;
	return HTMLEscapeElement.innerHTML;
}
function HTMLUnescape(str) {
	HTMLEscapeElement.innerHTML = str;
	return HTMLEscapeElement.textContent;
}

// Work around chrome bug
// https://code.google.com/p/chromium/issues/detail?id=401699
NodeList.prototype[Symbol.iterator] = Array.prototype[Symbol.iterator];
HTMLCollection.prototype[Symbol.iterator] = Array.prototype[Symbol.iterator];

function rescale_array(node, size) {
	let template = node.children[0];
	for(var i = node.children.length; i < size + 1; ++i) {
		let inst = template.cloneNode(true);
		inst.removeAttribute('hidden');
		node.appendChild(inst);
	}
	while(node.children.length > size + 1)
		node.removeChild(node.children[node.children.length - 1]);
}

// Super simple template engine
function join(node, data) {
	if(node === undefined || data === undefined)
		return;
	if(node.constructor === String)
		return join(document.getElementById(node), data);
	if(node.dataset !== undefined) {
		if(node.dataset.value !== undefined)
			return node.value = node.dataset.value === '' ? data : data[node.dataset.value];
		if(node.dataset.content !== undefined)
			return node.textContent = node.dataset.content === '' ? data : data[node.dataset.content];
		if(node.dataset.join !== undefined)
			return eval('(function(data){'+node.dataset.join+'})').apply(node, [data]);
		if(node.dataset.array !== undefined) {
			let a = node.dataset.array === '' ? data : data[node.dataset.array];
			if(!(a instanceof Array))
				return;
			rescale_array(node, a.length);
			for(var i = 0; i < a.length; ++i)
				join(node.children[i + 1], a[i]);
			return;
		}
	}
	if(node.join !== undefined && node.join !== Node.prototype.join)
		return node.join(data);
	for(let child of node.children)
		join(child, data);
}
function extract(node) {
	function objectify(name, value) {
		if(name === '')
			return value;
		let r = {};
		r[name] = value;
		return r;
	}
	if(node === undefined)
		return undefined;
	if(node.constructor == String)
		return extract(document.getElementById(node));
	if(node.dataset !== undefined) {
		if(node.dataset.value !== undefined)
			return objectify(node.dataset.value, node.value);
		if(node.dataset.content !== undefined)
			return objectify(node.dataset.content, node.textContent);
		if(node.dataset.extract !== undefined)
			return eval('(function(){'+node.dataset.extract+'})').apply(node, []);
		if(node.dataset.array !== undefined) {
			let a = [];
			for(var i = 1; i < node.children.length; ++i)
				a.push(extract(node.children[i]));
			return objectify(node.dataset.array, a);
		}
	}
	if(node.extract && node.extract !== Node.prototype.extract)
		return node.extract();
	let acc = undefined;
	for(let child of node.children) {
		let data = extract(child);
		if(data === undefined)
			continue;
		if(data instanceof Array)
			return data;
		if(!(data instanceof Object))
			return data;
		if(acc === undefined)
			acc = {};
		acc = Object.assign(acc, data);
	}
	return acc;
}
Node.prototype.join = function(data) { join(this, data); }
Node.prototype.extract = function() { return extract(this); }

// Fancy draggable elements
function drag(node, e) {
	let parent = node.parentNode;
	let was_dragged = false;
	let bounds = node.getBoundingClientRect();
	let grabX = (e.clientX - bounds.left) / bounds.width;
	let grabY = (e.clientY - bounds.top) / bounds.height;
	let old_index = Array.prototype.indexOf.call(parent.children, node);
	
	// Create a draggable clone
	let clone = node.cloneNode(true);
	clone.className += ' drag';
	clone.style.left = e.clientX - grabX * bounds.width + 'px';
	clone.style.top = e.clientY - grabY * bounds.height + 'px';
	clone.style.width = bounds.width + 'px';
	clone.style.height = bounds.height + 'px';
	parent.appendChild(clone);
	
	// Turn the element into a placeholder
	let placeholder = document.createElement('placeholder');
	let placeholder2 = document.createElement('placeholder');
	placeholder.appendChild(placeholder2);
	node.appendChild(placeholder);
	node.className += ' placeholder';
	
	// Enable wiggle on the parent
	parent.className += ' wiggle';
	
	// Install mouse event handlers
	let mousemove = function(e) {
		
		// We are now officially dragging
		// Todo: require some initial offset
		was_dragged = true;
		
		// Find element under cursor excluding the drag item.
		clone.style.visibility = 'hidden';
		let hover = document.elementFromPoint(e.clientX, e.clientY);
		clone.style.visibility = 'visible';
		while(hover && hover.parentNode !== parent) {
			hover = hover.parentNode;
		}
		if(hover && hover != node) {
			// Swap node and hover
			let next = hover.nextSibling;
			if(next === node) {
				parent.insertBefore(node, hover);
			} else {
				parent.insertBefore(hover, node);
				if(next) {
					parent.insertBefore(node, next);
				} else {
					parent.appendChild(node);
				}
			}
			
			// Update the draggable size
			bounds = node.getBoundingClientRect();
			clone.style.width = bounds.width + 'px';
			clone.style.height = bounds.height + 'px';
		}
		
		// Update the draggable location
		clone.style.left = e.clientX - grabX * bounds.width + 'px';
		clone.style.top = e.clientY - grabY * bounds.height + 'px';
	};
	let finish = function(e) {
		// Remove dragging stuff
		node.className = node.className.replace(/\bplaceholder\b/,'');
		parent.className = parent.className.replace(/\bwiggle\b/,'');
		parent.removeChild(clone);
		node.removeChild(placeholder);
		
		// If we were not dragging, it was a click
		if(!was_dragged)
			node.click(e);
		
		// If we changed places, call reorder handler
		if(parent.dataset !== undefined && parent.dataset.onreorder !== undefined) {
			try {
				let handler = eval(parent.dataset.onreorder);
				let new_index = Array.prototype.indexOf.call(parent.children, node);
				handler(old_index, new_index);
			}
			catch(e) {
				console.log(e);
			};
		}
		
		// Cancel event propagation
		e.preventDefault();
		e.stopPropagation();
		return false;
	}
	document.addEventListener('mousemove', mousemove, true);
	document.addEventListener('mouseup', function mouseup(e) {
		finish(e);
		document.removeEventListener('mousemove', mousemove, true);
		document.removeEventListener('mouseup', mouseup, true);
	}, true);
	
	// Cancel event propagation
	e.preventDefault();
	e.stopPropagation();
	return false;
}

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

document.addEventListener('DOMContentLoaded', function() {
	for(let el of document.getElementsByTagName('submit')) {
		console.log(el);
		el.onclick = function() { console.log(this.value); this.form.action = this.value; };
	}
});

// JSON API callback
function request(method, url, data, on_succes, on_error)
{
	console.log(method + ' ' + url);
	let r = new XMLHttpRequest();
	r.open(method, url, true);
	r.onreadystatechange = function () {
		if(r.readyState !== 4)
			return;
		if(r.status != 200) {
			if(typeof on_error === 'function')
				on_error(url, r.status, r);
			return;
		}
		on_succes(JSON.parse(r.responseText));
	};
	r.send(data === undefined ? data : JSON.stringify(data));
}
let get = function(url, on_succes, on_error) {
	request('GET', url, undefined, on_succes, on_error);
};
let post = request.bind(undefined, 'POST');
