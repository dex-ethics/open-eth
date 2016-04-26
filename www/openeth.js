"use strict";

function flesh_out(table) {
	var rows = table.length;
	if(rows === 0)
		return table;
	var cols = table[0].length;
	if(cols === 0)
		return table;
	for(var i = 0; i < rows; ++i) {
		for(var j = 0; j < cols; ++j) {
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
	if(positive.length == 0)
		return [];
	
	var n = positive[0].length;
	
	var possibleSigns = [];
	for(var i = 0; i < Math.pow(2, n); ++i) {
		
		// Create a signing
		var sign = [];
		for(var j = 0; j < n; ++j) {
			sign.push(((i & (1 << j)) != 0) ? -1 : 1);
		}
		
		// Apply the sign
		var pos = positive.map(function(e) {
			return e.map(function(e, i) {
				return e * sign[i];
			});
		});
		var neg = negative.map(function(e) {
			return e.map(function(e, i) {
				return e * sign[i];
			});
		});
		
		// Check the consistency
		// ∀ p ∊ pos ∀ n ∊ neg ∃ i nᵢ < pᵢ
		if(pos.every(function(p) {
			return neg.every(function(n) {
				return n.some(function(e, i) {
					return e < p[i];
				});
			});
		})) {
			possibleSigns.push(sign);
		}
	}
	if(possibleSigns.length == 0) {
		console.log("Inconsistent signs!");
		return [0].repeat(n);
	}
	
	// The result is zero if there there is ambiguity
	var result = possibleSigns[0];
	for_each(possibleSigns, function(signs) {
		for(var i = 0; i < n; ++i) {
			if(result[i] !== signs[i]) {
				result[i] = 0;
			}
		}
	});
	return result;
}

// Maximize the volume {x | x ≥ clause} s.t. forbidden ∩ volume = ∅
function maximizeVolume(clause, forbidden)
{
	// Check consistency
	// ∀ n ∊ forbidden ∃ i nᵢ < clauseᵢ
	if(!forbidden.every(function(n) {
		return n.some(function(e, i) {
			return e < clause[i];
		});
	})) {
		console.log("Inconsistent clause!");
		return undefined;
	}
	
	// For every permutation of the dimensions
	var options = Array.range(clause.length).permutations().map(function(order) {
		// Maximize the volume greedy one dimension at a time
		return order.reduce(function (option, dimension) {
			// Set the dimension to the minimal dimension of
			// the forbidden vectors that would fall inside the volume
			var bound = Math.max.apply(null,
				forbidden.filter(function(e) {
					return e.every(function(x, i) {
						return i === dimension || x >= option[i];
					})
				}).map(function(e) {
					return 1 + e[dimension];
				}));
			if(bound > option[dimension]) {
				console.log("Illegal bound in maximizeVolume");
			}
			option[dimension] = bound;
			return option;
		}, clause.clone());
		
	// Unique the solutions
	}).filter(function(e,i,a) {
		return i == a.findIndex(function(f) {
			return f.every(function(g, j) {
				return g == e[j];
			});
		});
	});
	
	// Prefer solutions with the least number of variables
	var variables = Math.min.apply(null, options.map(function(e) {
		return e.length - e.filter(function(f) {
			return f === -Infinity;
		}).length;
	}));
	options = options.filter(function(e) {
		return e.length - e.filter(function(f) {
			return f === -Infinity;
		}).length == variables;
	});
	return options;
}

function solve_dilemma(dilemma) {
	var a = dilemma.actions;
	var f = dilemma.features;
	var n = f.length;
	
	// Compute the positive and negative vectors
	var positive = [];
	var negative = [];
	for(var index = 0; index != dilemma.cases.length; ++index) {
		var c = dilemma.cases[index];
		
		// Skip cases without a correct action
		if(c.action === null)
			continue;
		
		var mat = c.features;
		var correct = c.action;
		var incorrect = 1 - c.action;
		
		// Δ = a₁ - a₂
		var delta = [];
		for(var i = 0; i < n; ++i) {
			delta[i] = mat[correct][i] - mat[incorrect][i];
		}
		
		// Positive: prefer(correct, incorrect) =  Δ
		// Negative: prefer(incorrect, correct) = -Δ
		positive.push(delta);
		negative.push(delta.map(function(e) {return -e;}));
	}
	
	// Find the signs, the prima facie duties
	var signs = findSigns(positive, negative);
	
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
	positive = positive.map(function(e) {
		return e.map(function(e, i) {
			return e * signs[i];
		});
	});
	negative = negative.map(function (e) {
		return e.map(function(e, i) {
			return e * signs[i];
		});
	});
	
	// Find different maximal volumes of positive clauses
	var clauses = positive.map(function(e) {
		return maximizeVolume(e, negative);
	});
	
	// If there are multiple solutions, find the simplest one
	function findSolutions(remaining) {
		if(remaining.length === 0) {
			return [[]];
		}
		var current = remaining[0];
		if(current === undefined) {
			return [[]];
		}
		var recurse = findSolutions(remaining.slice(1));
		var result = [];
		for_each(current, function(clause) {
			for_each(recurse, function(r) {
				result.push([clause].concat(r));
			});
		});
		return result;
	}
	var solutions = findSolutions(clauses);
	
	// Minimize the solutions by removing duplicate clauses
	solutions = solutions.map(function(s) {
		return s.unique(function(a,b) {
			return a.equals(b);
		});
	});
	
	// Find the solutions with the smallest number of clauses
	var num_clauses = Math.min.apply(null, solutions.map(function(s) {
		return s.length;
	}));
	solutions = solutions.filter(function(s) {
		return s.length === num_clauses;
	});
	if(solutions.length > 1) {
		console.log("TODO: Still some ambiguity remaining.");
	}
	var solution = solutions[0];
	
	// Print the clauses
	dilemma.principles = [];
	for_each(solution, function(clause) {
		var principle = [];
		var i = 0;
		for(var index = 0; index != clause.length; ++index) {
			var constraint = clause[index];
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
	});
}

function dilemma_map_actions(dilemma) {
	for(var i = 0; i != dilemma.cases.length; ++i)
		dilemma.cases[i].action = dilemma.actions[dilemma.cases[i].action];
}

function case_map_features(case_) {
	case_.features = flesh_out(case_.features);
	var features = [];
	for(var i = 0; i != case_.dilemma.features.length; ++i) {
		features.push({
			feature: case_.dilemma.features[i],
			values: [case_.features[0][i], case_.features[1][i]],
		});
	}
	case_.features = features;
}
