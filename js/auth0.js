'use strict';
(()=>{
	
	//
	// Authentication using Auth0
	//
	
	let client_id = 'AZmtkBN5zDGERJesFZGFS8vYJYyZTrDo';
	
	let lock = null;
	let with_lock = (action, failed)=>{
		
		if(lock !== null) {
			action(lock);
			return;
		}
		
		// Load Auth0 script
		let script = document.createElement('script');
		script.type = 'text/javascript';
		script.src = '//cdn.auth0.com/js/lock-8.2.min.js';
		
		// Then bind the event to the callback function.
		// There are several events for cross browser compatibility.
		script.onload = script.onreadystatechange = function(a,b,c,d){
			lock = new Auth0Lock(client_id, 'openeth.auth0.com');
			
			action(lock);
		}
		
		// TODO: Error handling
		
		// Fire the loading
		document.getElementsByTagName('body')[0].appendChild(script);
	}
	
	// Add onclick handlers to buttons
	window.addEventListener('DOMContentLoaded', ()=>{
		
		// Add the click handler
		let login = document.getElementById('btn-login');
		if(login instanceof HTMLButtonElement) {
			login.disabled = false;
			login.addEventListener('click', ()=>{
				with_lock((lock)=>{
					let config = {};
					config['icon'] = 'buddha.png';
					config['authParams'] = {};
					config['authParams']['scope'] = 'openid role userid';
					lock.show(config);
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
	});
	
	// Login using hash token
	if(window.location.hash.indexOf('id_token=') != -1
		|| window.location.hash.indexOf('error=') != -1) {
		with_lock((lock)=>{
			let hash = lock.parseHash(window.location.hash);
			if (hash) {
				if (hash.error) {
					console.log("There was an error logging in", hash.error);
					// TODO alert('There was an error: ' + hash.error + '\n' + hash.error_description);
					// (Alert is not allowed in our sandbox)
				} else {
					// Save the token in the session:
					localStorage.setItem('id_token', hash.id_token);
					
					// Remove the hash from the url
					window.location.hash = '';
					history.replaceState("", document.title,
						window.location.pathname + window.location.search);
				}
			}
		});
	}
	
	// TODO: https://auth0.com/docs/user-profile
	// https://auth0.com/docs/user-profile/normalized
	
})();
