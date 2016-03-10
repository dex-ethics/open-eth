'use strict';

//
// PostgREST
//
// Taken from: https://github.com/calebmer/postgrest-client

function Api(url) {
	this.url = url;
	this.request = function(method, path) {
		let r = {
			method: method,
			path: this.url + path,
			headers: {},
			query: {},
			then_handler: data => console.log(data),
			catch_handler: error => console.log(error),
			payload: null,
			match: function(query) {
				Object.keys(query).forEach(key => this.eq(key, query[key]));
				return this
			},
			select: function(select) {
				this.query['select'] = select.replace(/\s/g, '');
				return this
			},
			order: function(property, ascending, nullsFirst) {
				this.query['order'] = `${property}.${ascending ? 'asc' : 'desc'}.${nullsFirst ? 'nullsfirst' : 'nullslast'}`;
				return this
			},
			range: function(from, to) {
				this.headers['Range-Unit'] = 'items';
				this.headers['Range'] = (from || '0') + '-' + (to || '');
				return this;
			},
			single: function() {
				this.headers['Prefer'] = 'plurality=singular';
				return this;
			},
			representation: function() {
				this.headers['Prefer'] = 'return=representation';
				return this;
			},
			body: function(data) {
				this.payload = data;
				return this;
			},
			send: function() {
				if(this.xhr !== undefined) {
					return this;
				}
				
				// Construct the path
				this.path += '?';
				for(let key in this.query) {
					this.path += key + '=' + encodeURIComponent(this.query[key]) +'&';
				}
				
				// Construct the XHR
				this.xhr = new XMLHttpRequest();
				this.xhr.open(this.method, this.path, true);
				
				// Add the headers
				for(let key in this.headers) {
					this.xhr.setRequestHeader(key, this.headers[key]);
				}
				
				// Set the readyState handler
				let x = this.xhr;
				this.xhr.onreadystatechange = ()=>{
					if(this.xhr.readyState !== 4)
						return;
					if(this.xhr.status >= 200 && this.xhr.status < 300) {
						const body = (()=>{try{return JSON.parse(this.xhr.responseText)}catch(e){}})()||{};
						
						// Add range bounds
						const range = this.xhr.getResponseHeader('Content-Range');
						const regexp = /^(\d+)-(\d+)\/(\d+)$/
						if(Array.isArray(body) && range && regexp.test(range)) {
							const match = regexp.exec(range);
							body.from = parseInt(match[1], 10);
							body.to = parseInt(match[2], 10);
							body.fullLength = parseInt(match[3], 10);
						}
						
						// Call the handler
						this.then_handler(body);
					} else {
						let error = this.xhr.responseText;
						try {
							error = JSON.parse(error).message;
						}
						catch(e) {
						}
						this.catch_handler(error);
					}
				};
				
				// Send the request
				if(this.payload !== null) {
					this.xhr.setRequestHeader('Content-Type', 'application/json');
					
					// Work around https://github.com/begriffs/postgrest/issues/501
					function arrayConvert(object) {
						'use strict';
						if(Array.isArray(object)) {
							return '{' + object.map(arrayConvert).join(',') + '}';
						} else {
							return JSON.stringify(object);
						}
					}
					for(let key in this.payload) {
						if(Array.isArray(this.payload[key])) {
							this.payload[key] = arrayConvert(this.payload[key]);
						}
					}
					this.xhr.send(JSON.stringify(this.payload));
				} else {
					this.xhr.send();
				}
				return this;
			},
			then: function(callback) {
				this.then_handler = callback;
				return this.send();
			},
			catch: function(callback) {
				this.catch_handler = callback;
				return this.send();
			}
		}
		
		// Set default headers
		r.headers['Accept'] = 'application/json';
		r.headers['Prefer'] = path.startsWith('/rpc/') ? 'count=none' : '';
		r.headers['Authorization'] = localStorage.id_token ? 'Bearer ' + localStorage.id_token : '';
		
		// Quickly add all filters
		'eq gt lt gte lte like ilike is in not'.split(' ').map(filter =>
			r[filter] = (name, value) => {
				r.query[name] = `${filter}.${Array.isArray(value) ? value.join(',') : value}`
				return r;
			}
		)
		return r;
	};
	
	// Quickly add all methods
	'POST GET PATCH DELETE OPTIONS'.split(' ').map(method =>
		this[method.toLowerCase()] = (path => this.request(method, path))
	)
}

let api = new Api('/api/');
