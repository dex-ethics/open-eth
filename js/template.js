'use strict';

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
	if(node.constructor === String)
		return join(document.getElementById(node), data);
	if(node.dataset !== undefined) {
		if(node.dataset.value !== undefined) {
			if(node.dataset.value === '')
				return node.value = data;
			else if(node.dataset.value in data)
				return node.value = data[node.dataset.value];
		}
		if(node.dataset.content !== undefined) {
			if(node.dataset.content === '')
				return node.textContent = data;
			else if(node.dataset.content in data)
				return node.textContent = data[node.dataset.content];
		}
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
		if(value === "undefined")
			value = undefined;
		if(value === "null")
			value = null;
		if(name === '')
			return value;
		let r = {};
		r[name] = value;
		return r;
	}
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
