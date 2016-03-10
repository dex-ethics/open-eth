'use strict';

function make_unique(existing, name) {
	
	// If name doesn't exist, we are done
	if(!existing.includes(name)) {
		return name;
	}
	
	// Remove the " (n)" suffix from name
	name = name.replace(/ \(\d+\)$/, '');
	
	// Find the highest " (n)" suffix
	let n = 0;
	let r = new RegExp("^" + RegExp.escape(name) + " \\((\\d+)\\)$");
	for(let s of existing) {
		let matches = s.match(r);
		if(matches !== null) {
			n = Math.max(n, matches[1]);
		}
	}
	
	// Append " (n+1)"
	name = name + " (" + (n + 1) + ")";
	
	// Toast notify
	// Materialize.toast("Renamed to <em>" + HTMLEscape(name) +"</em>.", 4000);
	console.log("Renamed to <em>" + HTMLEscape(name) +"</em>.");
	return name;
}

let main = document.getElementById('main');
let actions = document.getElementById('actions');
let cases = document.getElementById('cases');
let features = document.getElementById('features');
let guidance = document.getElementById('guidance');
let duties = document.getElementById('duties');
let principles = document.getElementById('principles');


main.form = document.getElementById('edit_dilemma');

main.edit = function() {
	
	let original = extract(main);
	join(main.form, original);
	show_modal(main.form, function() {
		let edited = extract(main.form);
		
		join(main, edited);
	});
}

actions.form = document.getElementById('edit_action');

actions.name = function(index) {
	return extract(actions).actions[index];
}

actions.index = function(name) {
	return extract(actions).actions.indexOf(name);
}

actions.edit = function(node) {
	let original = extract(node);
	let original_index = actions.index(original);
	let original_actions = extract(actions).actions;
	let others = original_actions.slice();
	others.splice(original_index, 1);
	
	join(actions.form, original);
	show_modal(actions.form, function() {
		let edited = extract(actions.form);
		
		// Trim leading/trailing white space
		edited = edited.trim();
		
		// The empty string is not allowed
		if(edited === "") {
			console.log("Action name can not be empty.", 4000);
			return;
		}
		
		// No-op fast path
		if(edited === original) {
			return;
		}
		
		// If the name is not unique, append a " (n)"
		edited = make_unique(others, edited);
		
		// Commit
		join(node, edited);
		
		// Update in the cases
		// TODO Empty strings also match the array template
		for(let e of cases.querySelectorAll('.action')) {
			if(e.textContent === original) {
				e.textContent = edited;
			}
		}
		
		// Update the principles
		principles.update();
	});
}

actions.reorder = function(old_index, new_index) {
	// This event handler is triggered by dragging the items in the list.
	// The list itself is already updated. But we still need to update
	// the matrices.
	old_index -= 1;
	new_index -= 1;
	for(let e of cases.getElementsByTagName('case')) {
		if(e.hasAttribute('hidden')) {
			continue;
		}
		let div = e.getElementsByTagName('div')[0];
		let matrix = extract(div);
		matrix.features.reorder(old_index, new_index);
		join(div, matrix);
	}
	
	// Update the principles
	principles.update();
}

features.form = document.getElementById('edit_feature');

features.name = function (index) {
	return extract(features).features[index];
}
features.index = function (name) {
	return extract(features).features.indexOf(name);
}

features.edit = function (node) {
	let original = extract(node);
	let original_index = features.index(original);
	let original_features = extract(features).features;
	let others = original_features.slice();
	others.splice(original_index, 1);
	
	join(features.form, extract(node));
	show_modal(features.form, function(action) {
		
		// User clicked [remove]
		if(action === "remove") {
			return features.remove(node);
		}
		
		// Extract the result
		let edited = extract(features.form);
		
		// Trim leading/trailing white space
		edited = edited.trim();
		
		// The empty string is not allowed
		if(edited === "") {
			Materialize.toast("Feature name can not be empty.", 4000);
			return;
		}
		
		// No-op fast path
		if(edited === original) {
			return;
		}
		
		// If the name is not unique, append a " (n)"
		edited = make_unique(others, edited);
		
		// Commit
		join(node, edited);
		
		// Update the principles
		principles.update();
	});
}

features.add = function () {
	let others = extract(features).features;
	
	join(features.form, "");
	
	// TODO: Set the form to a 'new feature' mode.
	//       This should remove the [remove] button
	//       and probably change [ok] to [add].
	show_modal(features.form, function(action) {
		
		
		// Extract the result
		let edited = extract(features.form);
		
		// Trim leading/trailing white space
		edited = edited.trim();
		
		// The empty string is not allowed
		if(edited === "") {
			Materialize.toast("Feature name can not be empty.", 4000);
			return;
		}
		
		// If the name is not unique, append a " (n)"
		edited = make_unique(others, edited);
		
		// Commit
		let inst = features.children[0].cloneNode(true);
		inst.removeAttribute('hidden');
		join(inst, edited);
		features.appendChild(inst);
		
		// Update the case matrices
		for(let e of cases.getElementsByTagName('case')) {
			if(e.hasAttribute('hidden')) {
				continue;
			}
			let div = e.getElementsByTagName('div')[0];
			let matrix = extract(div);
			matrix.features.forEach(function (e){ e.insert(index, null); });
			join(div, matrix);
		}
		
		// Update the principles
		principles.update();
	});
}

features.remove = function(node) {
	let index = Array.prototype.indexOf.call(node.parentNode.children, node) - 1;
	node.parentNode.removeChild(node);
	
	// Update the case matrices
	for(let e of cases.getElementsByTagName('case')) {
		if(e.hasAttribute('hidden')) {
			continue;
		}
		let div = e.getElementsByTagName('div')[0];
		let matrix = extract(div);
		matrix.features.forEach(function (e){ e.remove(index); });
		join(div, matrix);
	}
	
	// Update the principles
	principles.update();
}

features.reorder = function(old_index, new_index) {
	// This event handler is triggered by dragging the items in the list.
	// The list itself is already updated. But we still need to update
	// the matrices.
	old_index -= 1;
	new_index -= 1;
	for(let e of cases.getElementsByTagName('case')) {
		if(e.hasAttribute('hidden')) {
			continue;
		}
		let div = e.getElementsByTagName('div')[0];
		let matrix = extract(div);
		matrix.features.forEach(function (e){ e.reorder(old_index, new_index); });
		join(div, matrix);
	}
	
	// Update the principles
	principles.update();
}

cases.form = document.getElementById('edit_case');

cases.edit = function(node) {
	let data = extract(node);
	
	join(cases.form, data);
	show_fullscreen(cases.form, function(action) {
		if(action === 'remove') {
			return cases.remove(node);
		}
		
		// TODO name treatment
		
		let data = extract(cases.form);
		join(node, data);
		
		// Update the principles
		principles.update();
	});
}

cases.add = function() {
	let existing = extract(cases).cases.map(function (o){ return o.name; });
	let a = extract(actions).actions;
	let f = extract(features).features;
	
	join(cases.form, {
		id: null,
		name: "New Case",
		description: "",
		action: undefined,
		features: [[null].repeat(f.length)].repeat(a.length)
	});
	
	show_fullscreen(cases.form, function(action) {
		
		// Extract the result
		let edited = extract(cases.form);
		console.log(edited);
		
		// Trim leading/trailing white space
		edited.name = edited.name.trim();
		
		// The empty string is not allowed
		if(edited.name === "") {
			edited.name = "New Case";
		}
		
		// If the name is not unique, append a " (n)"
		edited.name = make_unique(existing, edited.name);
		
		// Commit
		let inst = cases.children[0].cloneNode(true);
		inst.removeAttribute('hidden');
		join(inst, edited);
		cases.appendChild(inst);
		
		// Update the principles
		principles.update();
	});
}

cases.remove = function(node) {
	node.parentNode.removeChild(node);
	
	// Update the principles
	principles.update();
}

cases.join_features = function(data) {
	let a = extract(actions).actions;
	let f = extract(features).features;
	let root = document.getElementById('case_features');
	
	rescale_array(root, a.length);
	for(var i = 0; i < a.length; ++i) {
		let e = root.children[i + 1];
		let legend = e.getElementsByTagName('legend')[0];
		let div = e.getElementsByTagName('div')[0];
		legend.textContent = a[i];
		rescale_array(div, f.length);
		for(var j = 0; j < f.length; ++j) {
			let k = div.children[j + 1];
			let d = data.features[i][j];
			let checkbox = k.children[0];
			let label = k.children[1];
			let number = k.children[2];
			let range = k.children[3];
			label.textContent = f[j];
			if(d === null) {
				checkbox.checked = false;
				range.value = 0;
				range.disabled = true;
				number.textContent = "";
			} else {
				checkbox.checked = true;
				range.value = d;
				range.disabled = false;
				number.textContent = d;
			}
		}
	}
}

cases.extract_features = function() {
	let root = document.getElementById('case_features');
	
	let r = [];
	for(var i = 1; i < root.children.length; ++i) {
		let e = root.children[i];
		let rr = [];
		let div = e.getElementsByTagName('div')[0];
		for(var j = 1; j < div.children.length; ++j) {
			let k = div.children[j];
			let checkbox = k.children[0];
			let range = k.children[3];
			if(checkbox.checked) {
				rr.push(parseInt(range.value));
			} else {
				rr.push(null);
			}
		}
		r.push(rr);
	}
	return {features: r};
}


cases.change_checkbox = function(node) {
	let label = node.parentNode;
	let checkbox = label.children[0];
	let span = label.children[1];
	let number = label.children[2];
	let range = label.children[3];
	
	if(checkbox.checked) {
		number.textContent = range.value;
		range.disabled = false;
	} else {
		number.textContent = "";
		range.disabled = true;
	}
	
	cases.update_table(extract(cases.form));
}

cases.change_range = function(node) {
	let label = node.parentNode;
	let checkbox = label.children[0];
	let span = label.children[1];
	let number = label.children[2];
	let range = label.children[3];
	
	number.textContent = range.value;
	
	cases.update_table(extract(cases.form));
}

function flesh_out(table) {
	let rows = table.length;
	if(rows === 0)
		return table;
	let cols = table[0].length;
	if(cols === 0)
		return table;
	for(let i = 0; i < rows; ++i) {
		for(let j = 0; j < cols; ++j) {
			if(table[i][j] === null) {
				table[i][j] = -table[rows - i - 1][j];
			}
			if(table[i][j] === null) {
				table[i][j] = 0;
			}
		}
	}
	return table;
}

cases.update_table = function(data) {
	data = flesh_out(data.features);
	let inputs = document.getElementById('case_features');
	let table = document.getElementById('cases_table');
	let thead = table.getElementsByTagName('thead')[0];
	let tbody = table.getElementsByTagName('tbody')[0];
	
	let action_labels = extract(actions).actions;
	let feature_labels = extract(features).features;
	
	while(thead.hasChildNodes()) {
		thead.removeChild(thead.lastChild);
	}
	while(tbody.hasChildNodes()) {
		tbody.removeChild(tbody.lastChild);
	}
	
	let tr = document.createElement('tr');
	tr.appendChild(document.createElement('td'));
	for(let f of feature_labels) {
		let th = document.createElement('th');
		th.textContent = f;
		tr.appendChild(th);
	}
	thead.appendChild(tr);
	let i = 0;
	for(let a of action_labels) {
		let tr = document.createElement('tr');
		let th = document.createElement('th');
		th.textContent = a;
		tr.appendChild(th);
		let j = 0;
		for(let f of feature_labels) {
			let td = document.createElement('td');
			td.textContent = data[i][j];
			tr.appendChild(td);
			++j;
		}
		tbody.appendChild(tr);
		++i;
	}
}

Array.prototype.greaterOrEqualThan = function(other) {
	return this.every((e, i) => e >= other[i]);
}

Array.prototype.zip = function(other) {
	let result = [];
	for(let i = 0; i < this.length; ++i) {
		result.push([this[i], other[i]]);
	}
	return result;
}

function findSigns(positive, negative) {
	let n = positive[0].length;
	
	let possibleSigns = [];
	for(let i = 0; i < Math.pow(2, n); ++i) {
		
		// Create a signing
		let sign = [];
		for(let j = 0; j < n; ++j) {
			sign.push(((i & (1 << j)) != 0) ? -1 : 1);
		}
		
		// Apply the sign
		let pos = positive.map(e => e.map((e, i) => e * sign[i]));
		let neg = negative.map(e => e.map((e, i) => e * sign[i]));
		
		// Check the consistency
		// ∀ p ∊ pos ∀ n ∊ neg ∃ i nᵢ < pᵢ
		if(pos.every(p => neg.every(n => n.some((e, i) => e < p[i])))) {
			possibleSigns.push(sign);
		}
	}
	if(possibleSigns.length == 0) {
		console.log("Inconsistent signs!");
	}
	
	// The result is zero if there there is ambiguity
	let result = possibleSigns[0];
	for(let signs of possibleSigns) {
		for(let i = 0; i < n; ++i) {
			if(result[i] !== signs[i]) {
				result[i] = 0;
			}
		}
	}
	return result;
}

// Maximize the volume {x | x ≥ clause} s.t. forbidden ∩ volume = ∅
function maximizeVolume(clause, forbidden)
{
	// Check consistency
	// ∀ n ∊ forbidden ∃ i nᵢ < clauseᵢ
	if(!forbidden.every(n => n.some((e, i) => e < clause[i]))) {
		console.log("Inconsistent clause!");
		return undefined;
	}
	
	// For every permutation of the dimensions
	let options = Array.range(clause.length).permutations().map(order => 
		// Maximize the volume greedy one dimension at a time
		order.reduce(function (option, dimension) {
			// Set the dimension to the minimal dimension of
			// the forbidden vectors that would fall inside the volume
			let bound = Math.max.apply(null,
				forbidden.filter(e =>
					e.every((x, i) => i === dimension || x >= option[i])
				).map(e => 1 + e[dimension]));
			if(bound > option[dimension]) {
				console.log("Illegal bound in maximizeVolume");
			}
			option[dimension] = bound;
			return option;
		}, clause.clone())
		
	// Unique the solutions
	).filter((e,i,a) => i == a.findIndex(f => f.every((g, j) => g == e[j])));
	
	// Prefer solutions with the least number of variables
	let variables = Math.min.apply(null, options.map(e => e.length - e.filter(f => f === -Infinity).length));
	options = options.filter(e => e.length - e.filter(f => f === -Infinity).length == variables);
	return options;
}

principles.update = function() {
	let a = extract(actions).actions;
	let f = extract(features).features;
	let n = f.length;
	
	// Compute the positive and negative vectors
	let positive = [];
	let negative = [];
	let data = extract(cases);
	for(let c of data.cases) {
		let mat = flesh_out(c.features);
		
		let correct = c.action;
		let incorrect = 1 - c.action;
		
		// Δ = a₁ - a₂
		let delta = [];
		for(let i = 0; i < n; ++i) {
			delta[i] = mat[correct][i] - mat[incorrect][i];
		}
		// Positive: prefer(correct, incorrect) =  Δ
		// Negative: prefer(incorrect, correct) = -Δ
		positive.push(delta);
		negative.push(delta.map(e => -e));
	}
	
	// Find the signs, the prima facie duties
	let signs = findSigns(positive, negative);
	
	// Print the duties
	duties.innerHTML = "<ul><li>It can be inferred that there are (prima facie) duties to <ul>"
		+ signs.map((e, i) => e === 0 ? undefined : ((e < 0 ? "minimize " : "maximize ") + f[i]))
		.filter(e => e !== undefined).map(e => "<li>" + e + "</li>").join() + "</ul></li></ul>";
	
	// Give guidance on ambiguity
	guidance.innerHTML = signs.map((e,i) => e !== 0 ? "" :
		("<p>Could not determine if " + f[i] + " is a good or a bad thing.</p>")).join();
	
	// Apply signs
	positive = positive.map(e => e.map((e, i) => e * signs[i]));
	negative = negative.map(e => e.map((e, i) => e * signs[i]));
	
	// Find different maximal volumes of positive clauses
	let clauses = positive.map(e => maximizeVolume(e, negative));
	
	// If there are multiple solutions, find the simplest one
	function findSolutions(remaining) {
		if(remaining.length === 0) {
			return [[]];
		}
		let current = remaining[0];
		let recurse = findSolutions(remaining.slice(1));
		let result = [];
		for(let clause of current) {
			for(let r of recurse) {
				result.push([clause].concat(r));
			}
		}
		return result;
	}
	let solutions = findSolutions(clauses);
	
	// Minimize the solutions by removing duplicate clauses
	solutions = solutions.map(s => s.unique((a,b) => a.equals(b)));
	
	// Find the solutions with the smallest number of clauses
	let num_clauses = Math.min.apply(null, solutions.map(s => s.length));
	solutions = solutions.filter(s => s.length === num_clauses);
	if(solutions.length > 1) {
		console.log("Still some ambiguity remaining.");
	}
	let solution = solutions[0];
	
	// Print the clauses
	let principle = "<ul>";
	for(let clause of solution) {
		principle += "<li>An action is ethically preferable to another if it <ul>"
		let i = 0;
		for(let constraint of clause) {
			if(constraint === -Infinity) {
				++i;
				continue;
			}
			principle += "<li>";
			if(constraint > 0) {
				principle += "satisfies the duty to "
			} else {
				principle += "does not violate the duty to "
			}
			if(signs[i] < 0) {
				principle += "minimize "
			} else {
				principle += "maximize "
			}
			principle += f[i];
			principle += " by a value ";
			if(constraint > 0) {
				principle += "at least "
			} else {
				principle += "greater than "
			}
			principle += Math.abs(constraint)
			principle += " more"
			principle += "</li>";
			++i;
		}
		principle += "</ul></li>"
	}
	principles.innerHTML = principle;
}

function debug()
{
	let data = extract('main');
	let form = document.getElementById('debug');
	join(form, data);
	show_modal(form);
}

function test()
{
	let data1 = extract('main');
	join('main', data1);
	let data2 = extract('main');
	console.log(data1, data2);
	if(JSON.stringify(data1) !== JSON.stringify(data2)) {
		console.log("Test failed");
	}
}

function load(url) {
	get(url, function(data) {
		join('main', data);
		
		// Update the principles
		principles.update();
	});
}


window.addEventListener('DOMContentLoaded', function() {
	// Load an empty case
	reset();
	
	// Load the examples
	api.get('dilemmas').select('id, name').order('id', true).range(0,4)
	.then(result => join('examples', result));
});

function load(id) {
	api.get('dilemmas').eq('id', id).select('*,cases{*}').single()
	.then(dilemma => {
		
		// TODO: Sort the cases
		
		join('main', dilemma);
		principles.update();
	});
}

function store() {
	let d = extract('main');
	
	if(d.id) {
		// Update
		api.patch('dilemmas').eq('id', d.id).body({
			name: d.name,
			description: d.description,
			actions: d.actions,
			features: d.features
		}).send();
		d.cases.forEach((c, i)=>{
			if(c.id) {
				api.patch('cases').eq('id', c.id).body({
					position: i,
					name: c.name,
					action: c.action,
					features: c.features
				}).send();
			} else {
				api.post('cases').representation().select('id').body({
					dilemma: d.id,
					position: i,
					name: c.name,
					action: c.action,
					features: c.features
				}).then(r => c.id = r.id);
			}
		});
	} else {
		// Insert
		api.post('dilemmas').representation().select('id').body({
			name: d.name,
			description: d.description,
			actions: d.actions,
			features: d.features
		}).then(id => {
			console.log(id);
			d.id = id;
			d.cases.forEach((c, i)=>{
				api.post('cases').eq('id', c.id).body({
					position: i,
					name: c.name,
					action: c.action,
					features: c.features
				}).then(id => c.id = id);
			});
			
			console.log(d);
		});
		
		// Feed the id's back up the chain
		// join('main', d);
	}
}

function delete_() {
	let d = extract('main');
	if(d.id) {
		api.delete('dilemmas').eq('id', d.id).send();
	}
	reset();
}

function reset() {
	join('main', {
		id: null,
		name: "New Dilemma",
		description: "Please edit this dilemma.",
		actions: ["Action A", "Action B"],
		features: ["Feature 1"],
		cases: []
	});
}
