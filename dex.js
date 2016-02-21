'use strict';

// Work around chrome bug
// https://code.google.com/p/chromium/issues/detail?id=401699
HTMLCollection.prototype[Symbol.iterator] = Array.prototype[Symbol.iterator];

function join(node, data) {
	if(node === undefined || data === undefined)
		return;
	else if(node.constructor == String)
		join(document.getElementById(node), data);
	else if(data.constructor === String) {
		if(node instanceof HTMLInputElement || node instanceof HTMLTextAreaElement)
			node.value = data;
		else
			node.textContent = data;
	} else if(data instanceof Array) {
		for(var i = node.children.length; i < data.length + 1; ++i) {
			let inst = node.children[0].cloneNode(true);
			inst.removeAttribute('hidden');
			node.appendChild(inst);
		}
		while(node.children.length > data.length + 1)
			node.removeChild(node.children[node.children.length - 1]);
		for(var i = 0; i < data.length; ++i)
			join(node.children[i + 1], data[i]);
	} else if(node.dataset && node.dataset.template)
		join(node, data[node.dataset.template]);
	else if(node.children) {
		for(let child of node.children)
			join(child, data);
	}
}

function extract(node)
{
	if(node === undefined)
		return;
	else if(node.constructor == String)
		return extract(document.getElementById(node));
	else if(node.dataset && node.dataset.template) {
		let r = {};
		if(node.children.length === 0) {
			if(node instanceof HTMLInputElement || node instanceof HTMLTextAreaElement)
				r[node.dataset.template] = node.value;
			else
				r[node.dataset.template] = node.textContent;
		} else {
			r[node.dataset.template] = [];
			for(var i = 1; i < node.children.length; ++i)
				r[node.dataset.template].push(extract(node.children[i]));
		}
		return r;
	} else if(node.children) {
		let r = {};
		for(let child of node.children) {
			let obj = extract(child);
			for(var attr in obj) {
				// TODO recursive merge
				r[attr] = obj[attr];
			}
		}
		return r;
	} else {
		return {};
	}
}

function drag(node, e) {
	let was_dragged = false;
	let bounds = node.getBoundingClientRect();
	let grabX = (e.clientX - bounds.left) / bounds.width;
	let grabY = (e.clientY - bounds.top) / bounds.height;
	
	// Enable wiggle on the parent
	let parent = node.parentNode;
	parent.className += ' wiggle';
	
	// Create a draggable clone
	let clone = node.cloneNode(true);
	clone.className += ' drag';
	clone.style.left = e.pageX - grabX * bounds.width - 10 + 'px';
	clone.style.top = e.pageY - grabY * bounds.height - 10 + 'px';
	clone.style.width = bounds.width + 'px';
	clone.style.height = bounds.height + 'px';
	parent.appendChild(clone);
	
	// Turn the element into a placeholder
	let placeholder = document.createElement('placeholder');
	let placeholder2 = document.createElement('placeholder');
	placeholder.appendChild(placeholder2);
	node.appendChild(placeholder);
	node.className += ' placeholder';
	
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
		clone.style.left = e.pageX - grabX * bounds.width - 10 + 'px';
		clone.style.top = e.pageY - grabY * bounds.height - 10 + 'px';
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
	}
	document.addEventListener('mousemove', mousemove, true);
	document.addEventListener('mouseup', function mouseup(e) {
		finish(e);
		document.removeEventListener('mousemove', mousemove, true);
		document.removeEventListener('mouseup', mouseup, true);
	}, true);
}

function show_modal(form, on_accept, on_reject)
{
	if(form.constructor == String)
		form = document.getElementById(form);
	let body = document.getElementsByTagName('body')[0];
	let backdrop = document.getElementById('modal_backdrop');
	if(backdrop === null) {
		backdrop = document.createElement('div');
		backdrop.id = 'modal_backdrop';
		body.appendChild(backdrop);
	}
	form.className += ' modal_dialog';
	form.removeAttribute('hidden');
	backdrop.removeAttribute('hidden');
	body.style.overflow = 'hidden';
	
	let backdrop_click = function() { form.reset(); };
	let form_reset;
	let form_submit;
	let close = function(accept) {
		form.className = form.className.replace(/\bmodal_dialog\b/,'');
		form.setAttribute('hidden','');
		backdrop.setAttribute('hidden','');
		body.style.overflow = '';
		backdrop.removeEventListener('click', backdrop_click, true);
		form.removeEventListener('reset', form_reset, true);
		form.removeEventListener('submit', form_submit, true);
		if(accept && typeof on_accept === 'function')
			on_accept();
		if(!accept && typeof on_reject === 'function')
			on_reject();
	};
	form_reset = close.bind(null, false);
	form_submit = close.bind(null, true);
	backdrop.addEventListener('click', backdrop_click, true);
	form.addEventListener('reset', form_reset, true);
	form.addEventListener('submit', form_submit, true);
}

function show_fullscreen(form, on_accept, on_reject)
{
	if(form.constructor == String)
		form = document.getElementById(form);
	let body = document.getElementsByTagName('body')[0];
	form.className += ' fullscreen_dialog';
	form.removeAttribute('hidden');
	body.style.overflow = 'hidden';
	
	let form_reset;
	let form_submit;
	let close = function(accept) {
		form.className = form.className.replace(/\bfullscreen_dialog\b/,'');
		form.setAttribute('hidden','');
		body.style.overflow = '';
		form.removeEventListener('reset', form_reset, true);
		form.removeEventListener('submit', form_submit, true);
		if(accept && typeof on_accept === 'function')
			on_accept();
		if(!accept && typeof on_reject === 'function')
			on_reject();
	};
	form_reset = close.bind(null, false);
	form_submit = close.bind(null, true);
	form.addEventListener('reset', form_reset, true);
	form.addEventListener('submit', form_submit, true);
}

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
