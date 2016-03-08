'use strict';
// Author: Remco Bloemen



// CSS vendor prefixes

// CSS hyphen polyfill
// https://leaverou.github.io/prefixfree/

RegExp.escape = function(str) {
	return String(str).replace(/([-()\[\]{}+?*.$\^|,:#<!\\])/g, "\\$1");
};

Array.prototype.clone = function(x) {
	return this.slice(0);
}

Array.prototype.remove = function(x) {
	return this.splice(x, 1)[0];
}

Array.prototype.insert = function(x, v) {
	this.splice(x, 0, v);
}

Array.prototype.swap = function (x,y) {
	let b = this[x];
	this[x] = this[y];
	this[y] = b;
	return this;
}

Array.prototype.repeat = function(times) {
	let arrays = [];
	for(let i =0; i < times; ++i) {
		arrays.push(this);
	}
	return Function.prototype.call.apply(Array.prototype.concat, arrays);
}

Array.range = function(a, b, s) {
	let start = b === undefined ? 0 : a;
	let end = b === undefined ? a : b;
	let step = s === undefined ? 1 : s;
	let result = [];
	for(let n = start; n < end; n += step) {
		result.push(n);
	}
	return result;
}

Array.prototype.permutations = function() {
	return this.reduce(function permute(res, item, key, arr) {
		return res.concat(arr.length > 1
			&& arr.slice(0, key)
				.concat(arr.slice(key + 1))
				.reduce(permute, [])
				.map(perm => [item].concat(perm))
			|| item);
	}, []);
}

Array.prototype.equals = function(other, equality) {
	if(equality === undefined) {
		equality = (a, b) => a === b;
	}
	return this.length === other.length && this.every((e, i) => equality(e, other[i]));
}

Array.prototype.unique = function(equality) {
	if(equality === undefined) {
		equality = (a, b) => a === b;
	}
	return this.filter((e,i,a) => i == a.findIndex(f => equality(e, f)));
}

// An improved join that understands a final_interjection to
// create sentences like "A, B, and C".
Array.prototype.join = function(interjection, final_interjection) {
	if(this.length == 0)
		return undefined;
	if(this.length == 1)
		return this[0];
	let result = this[0] !== undefined ? this[0] : '';
	for(let i = 1; i < this.length; ++i) {
		if(interjection !== undefined) {
			if(i < this.length - 1) {
				result += interjection;
			} else if(i === this.length - 1) {
				if(final_interjection !== undefined) {
					result += final_interjection;
				} else {
					result += interjection;
				}
			}
		}
		if(this[i] !== undefined) {
			result += this[i];
		}
	}
	return result;
}

Array.prototype.equals = function(other) {
	return this.every((e,i) => e == other[i]);
}

// Take element at position x and move it to position y, shifting
// all elements in between.
Array.prototype.reorder = function(from, to) {
	// @note this could also be done in place using a loop
	//       and a temp variable.
	// @todo test for to < from and from > to
	this.insert(to, this.remove(from));
}

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
window.data_onreorder_onmousedown = (node, e)=>{
	let parent = node.parentNode;
	let was_dragged = false;
	let bounds = node.getBoundingClientRect();
	let grabX = (e.clientX - bounds.left) / bounds.width;
	let grabY = (e.clientY - bounds.top) / bounds.height;
	let old_index = Array.prototype.indexOf.call(parent.children, node);
	
	// Create a draggable clone
	let clone = node.cloneNode(true);
	clone.className += ' data_onreorder_dragging';
	clone.style.left = e.clientX - grabX * bounds.width + 'px';
	clone.style.top = e.clientY - grabY * bounds.height + 'px';
	clone.style.width = bounds.width + 'px';
	clone.style.height = bounds.height + 'px';
	parent.appendChild(clone);
	
	// Turn the element into a placeholder
	let placeholder = document.createElement('div');
	let placeholder2 = document.createElement('div');
	node.className += ' data_onreorder_placeholder0';
	placeholder.className = 'data_onreorder_placeholder1';
	placeholder2.className = 'data_onreorder_placeholder2';
	placeholder.appendChild(placeholder2);
	node.appendChild(placeholder);
	
	// Enable wiggle on the parent
	parent.className += ' wiggle';
	
	// Install mouse event handlers
	let mousemove = (e)=>{
		
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
	let finish = (e)=>{
		// Remove dragging stuff
		node.className = node.className.replace(/\bdata_onreorder_placeholder0\b/,'');
		parent.className = parent.className.replace(/\bdata_onreorder_wiggle\b/,'');
		parent.removeChild(clone);
		node.removeChild(placeholder);
		
		// If we were not dragging, it was a click
		if(!was_dragged)
			node.click(e);
		
		// If we changed places, call reorder handler
		if(parent.dataset !== undefined && parent.dataset.onreorder !== undefined) {
			try {
				let handler = eval(parent.dataset.onreorder);
				if(typeof handler === "function") {
					let new_index = Array.prototype.indexOf.call(parent.children, node);
					handler(old_index, new_index);
				}
			}
			catch(e) {
				console.log(e);
			};
		}
		
		// Cancel event propagation
		e.preventDefault();
		e.stopPropagation();
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
};
window.addEventListener('DOMContentLoaded', ()=>{
	// We do this by setAttribute so the cloneNode() will copy it.
	for(let elm of document.querySelectorAll('[data-onreorder] >*'))
		elm.setAttribute('onmousedown', 'data_onreorder_onmousedown(this,event)');
});


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

let token = undefined;

// JSON API callback
function request(method, url, data, on_succes, on_error) {
	console.log(method + ' ' + url);
	let r = new XMLHttpRequest();
	r.open(method, url, true);
	r.onreadystatechange = function () {
		if(r.readyState !== 4)
			return;
		if(r.status != 200) {
			if(typeof on_error === 'function') {
				let message = r.responseText;
				try {
					message = JSON.parse(r.responseText).message;
				}
				catch(e) {
				}
				on_error(message, r.status, url, r);
			}
		} else {
			on_succes(JSON.parse(r.responseText));
		}
	};
	if(data !== undefined) {
		r.setRequestHeader('Content-Type', 'application/json');
	}
	if(token !== undefined) {
		r.setRequestHeader('Authorization', 'Bearer ' + token);
	}
	r.setRequestHeader('Prefer', 'count=none');
	r.setRequestHeader('Accept', 'application/json');
	// r.setRequestHeader('Range-Unit', 'items');
	// r.setRequestHeader('Range', '0-14');
	r.send(data === undefined ? data : JSON.stringify(data));
}
let get = function(url, on_succes, on_error) {
	request('GET', url, undefined, on_succes, on_error);
};
let post = request.bind(undefined, 'POST');


//
// Authentication using Auth0
//

(()=>{
	
	let client_id = 'AZmtkBN5zDGERJesFZGFS8vYJYyZTrDo';
	
	// Load Auth0 script
	let lock = undefined;
	let script = document.createElement('script');
	script.type = 'text/javascript';
	script.src = '//cdn.auth0.com/js/lock-8.2.js';
	
	// Then bind the event to the callback function.
	// There are several events for cross browser compatibility.
	script.onload = script.onreadystatechange = ()=>{
		lock = new Auth0Lock(client_id, 'openeth.auth0.com');
		
		// Add the click handler
		let login = document.getElementById('btn-login');
		if(login instanceof HTMLButtonElement) {
			login.disabled = false;
			login.addEventListener('click', ()=>{
				lock.show({
					icon: 'buddha.png',
					authParams: { scope: 'openid role' }
				});
			});
		}
		
		let logout = document.getElementById('btn-logout');
		if(logout instanceof HTMLButtonElement) {
			logout.disabled = true;
			logout.addEventListener('click', ()=>{
				localStorage.removeItem('id_token');
				window.location.href = "/";
			});
		}
		
		console.log(window.location.hash);
		let hash = lock.parseHash(window.location.hash);
		if (hash) {
			if (hash.error) {
				console.log("There was an error logging in", hash.error);
				alert('There was an error: ' + hash.error + '\n' + hash.error_description);
			} else {
				// Save the token in the session:
				localStorage.setItem('id_token', hash.id_token);
				localStorage.setItem('access_token', hash.access_token);
				
				console.log(hash);
				
				// Remove the hash from the url
				window.location.hash = '';
			}
		} else {
			console.log("There was no hash.");
		}
	};
	
	// Fire the loading
	document.getElementsByTagName('body')[0].appendChild(script);
	
})();

// TODO: https://auth0.com/docs/user-profile
// https://auth0.com/docs/user-profile/normalized
