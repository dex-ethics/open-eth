//
// Create and trigger custom events
//

function create_event(name) {
	return function(data) {
		var event = document.createEvent('CustomEvent');
		event.initCustomEvent(name, true, true, data);
		return document.dispatchEvent(event);
	}
}

//
// document.addEventListener('test', function(e) {
// 	console.log(e);
// });
//
// var test_event = create_event('test');
//
// test_event('haha');
//

//
// Authentication using Auth0
//
// <button id="auth0-login"></button>
// <button id="auth0-logout"></button>
// <button id="auth0-register"></button>
//
var user_log_in_try_event = create_event('log_in_try');
var user_log_in_event = create_event('log_in');
var user_log_out_event = create_event('log_out');
var user_token = null;
var user_profile = null;
var user_id = null;
var lock = new Auth0Lock('AZmtkBN5zDGERJesFZGFS8vYJYyZTrDo', 'openeth.auth0.com');
var lock_options = {
	icon: 'buddha.png',
	callbackURL: 'https://' + window.location.hostname + '/',
	responseType: 'token',
	authParams: {
		scope: 'openid role userid'
	}
};
lock.on('hidden', function() {
	if(user_id != null) {
		user_log_in_event();
	} else {
		user_log_out_event();
	}
});


// Fetch hash token if we have any
(function() {
	
	// Check if we have a hash token
	var auth_hash = lock.parseHash(window.location.hash);
	if(!auth_hash)
		return;
	
	// Check for error
	if(auth_hash.error) {
		var message = 'There was an error: '
			+ auth_hash.error + '\n';
			+ auth_hash.error_description;
		notify(message, 30, 'error');
		return;
	}
	
	// We have an authentication token
	notify("Received authentication token from hash.", 5, "debug");
	
	// Remove the hash from the url
	window.location.hash = '';
	window.history.replaceState("", document.title,
		window.location.pathname + window.location.search);
	
	// Remove old profile
	window.localStorage.removeItem('auth_profile');
	
	// Store the token for re-use
	window.localStorage.setItem('auth_token', auth_hash.id_token);
	
})();

// Use stored token
(function() {
	
	// Check if we have a stored token
	var auth_token = localStorage.getItem('auth_token');
	if(!auth_token)
		return;
	
	notify("Authentication token from local storage", 5, "debug");
	
	// Check if the token is expired
	var jwt = null;
	try {
		// Try existing token without calling out to servers
		jwt = lock.$auth0.decodeJwt(auth_token);
		var expires = jwt.exp * 1000;
		var treshold = Date.now() + 3600000;
		if(treshold >= expires) {
			
			// Notify user
			notify('Authentication token has expired', 10, 'info');
			
			// Erase invalid token
			window.localStorage.removeItem('auth_token');
			window.localStorage.removeItem('auth_profile');
			return;
		}
	} catch(error) {
		notify('Could not read authentication token.', 10, 'error');
		return;
	}
	
	// Proceed as if the token is valid (but verify)
	user_token = auth_token;
	user_id = jwt.userid;
	user_profile = JSON.parse(window.localStorage.getItem('auth_profile'));
	
	// Check if we can already trigger a login event.
	// This handles the case where a user refreshes the page.
	if(user_profile !== null) {
		notify('User logged in from localstore', 5, 'debug');
		user_log_in_event(user_profile);
	}
	
	// Fetch the user profile
	try {
		lock.getProfile(auth_token, function(error, profile) {
			
			// Detect state changes
			if(error)
				throw error;
			var logged_in = user_profile == null;
			var profile_updated = user_profile != profile;
			
			// Store profile
			window.localStorage.setItem('auth_profile', JSON.stringify(profile));
			user_profile = profile;
			
			// Send log in event if we haven't already
			if(logged_in) {
				notify('User logged in from profile', 5, 'debug');
				user_log_in_event(user_profile);
			}
			
			// Upsert user profile in database
			if(profile_updated) {
				api.post('/rpc/login')
				.body({user_profile: user_profile})
				.then(function(){})
				.send();
			}
			
		});
	} catch(error) {
		notify('Could not get user profile:\n' + error, 10, 'error');
		
		// Log out
		if(user_profile != null)
			user_log_out_event();
		window.localStorage.removeItem('auth_token');
		window.localStorage.removeItem('auth_profile');
		user_token = null;
		user_id = null;
		user_profile = null;
		return;
	}
	
})();

function register() {
	user_log_in_try_event();
	var clone = JSON.parse(JSON.stringify(lock_options));
	clone.authParams.state = window.location.href;
	lock.showSignup(clone);
}
function login() {
	user_log_in_try_event();
	var clone = JSON.parse(JSON.stringify(lock_options));
	clone.authParams.state = window.location.href;
	lock.show(clone);
}
function logout() {
	window.localStorage.removeItem('auth_token');
	window.localStorage.removeItem('auth_profile');
	user_token = null;
	user_id = null;
	user_profile = null;
	lock.logout({
		client_id: lock.$client.id,
		returnTo: window.location.href
	});
	user_log_out_event();
}
