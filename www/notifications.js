
function notify(message, timeout, msg_class, callback) {
	
	if(msg_class === undefined)
		msg_class = "info";
	if(timeout === undefined)
		timeout = 10;
	
	// Show all messages in the console.
	var prefix = msg_class.charAt(0).toUpperCase() + msg_class.slice(1);
	console.log(prefix + ": " + message);
	
	// Do not show debug messages in the UI, except when on staging.*
	var staging = window.location.hostname.startsWith("staging.");
	if(msg_class === "debug" && !staging) {
		if(callback !== undefined)
			window.setTimeout(callback, 100);
		return;
	}
	
	var notifications = document.getElementById('notifications');
	if(notifications === null) {
		notifications = document.createElement('div');
		notifications.setAttribute('id', 'notifications');
		document.body.appendChild(notifications);
	}
	var notification = document.createElement('div');
	var close_button = document.createElement('button');
	notification.classList.add('notification');
	notification.classList.add(msg_class);
	notification.textContent = message;
	close_button.textContent = "close";
	close_button.classList.add('close');
	notification.insertBefore(close_button, notification.firstChild);
	notifications.appendChild(notification);
	
	var closed = false;
	var close = function() {
		if(closed)
			return;
		closed = true;
		notification.classList.add('hide');
		if(callback !== undefined)
			callback();
		window.setTimeout(function(){
			notifications.removeChild(notification);
		}, 1000);
	};
	
	close_button.addEventListener('click', close);
	window.setTimeout(close, timeout * 1000);
};
