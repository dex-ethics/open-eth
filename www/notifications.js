
function notify(message, timeout, msg_class, callback) {
	
	if(msg_class === undefined)
		msg_class = "info";
	if(timeout === undefined)
		timeout = 10;
	
	console.log(msg_class + ": " + message);
	
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
	notification.appendChild(close_button);
	notifications.appendChild(notification);
	
	var closed = false;
	var close = function() {
		if(closed)
			return;
		closed = true;
		console.log("Removing..." + notification);
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
