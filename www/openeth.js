"use strict";

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

function solve_dilemma(dilemma) {
	var a = dilemma.actions;
	var f = dilemma.features;
	var n = f.length;
	
	// Compute the positive and negative vectors
	let positive = [];
	let negative = [];
	for(let c of dilemma.cases) {
		let mat = c.features;
		let correct = c.action;
		let incorrect = 1 - c.action;
		
		// Δ = a₁ - a₂
		var delta = [];
		for(var i = 0; i < n; ++i) {
			delta[i] = mat[correct][i] - mat[incorrect][i];
		}
		
		// Positive: prefer(correct, incorrect) =  Δ
		// Negative: prefer(incorrect, correct) = -Δ
		positive.push(delta);
		negative.push(delta.map(e => -e));
	}
	
	// Find the signs, the prima facie duties
	let signs = findSigns(positive, negative);
	
	// Store the duties
	dilemma.duties = signs.map(function(sign, i) {
		if(sign === 0)
			return undefined;
		else if(sign < 0)
			return "minimize " + f[i];
		else
			return "maximize " + f[i];
	}).filter(function(e){return e !== undefined});
	
	// Give guidance on ambiguity
	dilemma.guidance = signs.map(function(sign,i) {
		if(sign === 0)
			return "Could not determine if " + f[i] + " is a good or a bad thing.";
		else
			return undefined;
	}).filter(function(e){return e !== undefined});
	
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
		console.log("TODO: Still some ambiguity remaining.");
	}
	let solution = solutions[0];
	
	// Print the clauses
	dilemma.principles = [];
	for(let clause of solution) {
		var principle = [];
		let i = 0;
		for(let constraint of clause) {
			var clausestr = "";
			if(constraint === -Infinity) {
				++i;
				continue;
			}
			if(constraint > 0) {
				clausestr += "satisfies the duty to "
			} else {
				clausestr += "does not violate the duty to "
			}
			if(signs[i] < 0) {
				clausestr += "minimize "
			} else {
				clausestr += "maximize "
			}
			clausestr += f[i];
			clausestr += " by a value ";
			if(constraint > 0) {
				clausestr += "at least "
			} else {
				clausestr += "greater than "
			}
			clausestr += Math.abs(constraint)
			clausestr += " more"
			++i;
			principle.push(clausestr);
		}
		dilemma.principles.push({clauses: principle});
	}
}

function dilemma_map_actions(dilemma) {
	for(var i in dilemma.cases)
		dilemma.cases[i].action = dilemma.actions[dilemma.cases[i].action];
}

function dilemma_unmap_actions(dilemma) {
	for(var i in dilemma.cases)
		dilemma.cases[i].action = dilemma.actions.indexOf(dilemma.cases[i].action);
}
