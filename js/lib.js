'use strict';

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
