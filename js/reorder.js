'use strict';

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
