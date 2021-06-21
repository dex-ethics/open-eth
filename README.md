# Welcome to Open Eth!

You can play with ethics on [**legacy.ethicsnet.org**](http://legacy.ethicsnet.org/).

You can chat with the developers on [**gitter**](https://gitter.im/dex-ethics/open-eth).

You can contribute to Open Eth here on GitHub!


# OpenEth

## Dependencies

Make sure you have a recent version of `docker`, at least version *1.10.0*.

https://docs.docker.com/engine/installation/linux/ubuntulinux/

Make sure you have a recent version of `docker-compose`, at least version *1.6*.

	curl -L https://github.com/docker/compose/releases/download/1.6.2/docker-compose-`uname -s`-`uname -m` | sudo tee /usr/local/bin/docker-compose > /dev/null
	sudo chmod +x /usr/local/bin/docker-compose

## Getting the source code

	git clone https://github.com/dex-ethics/open-eth
	cd open-eth

## Configuring

	source site.conf
	export DOMAIN CERT_EMAIL JWT_SECRET POSTGRES_PASSWORD AUTHENTICATOR_PASSWORD

## Starting

	docker-compose up

## (Alternative) To start with a clean build

	docker-compose stop
	sudo rm -rf data/data
	docker-compose rm -f
	docker-compose create
	docker-compose start
	docker-compose logs
	docker-compose stop
	sleep 10
	docker-compose start

## Interact

	xdg-open http://localhost:8080

To log in, open javascript console and enter:

```
user_token= 'eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJyb2xlIjoiYXV0aG9yIiwidXNlcmlkIjoiZGV2LXRlc3QiLCJpc3MiOiJodHRwczovL29wZW5ldGguYXV0aDAuY29tLyIsInN1YiI6ImF1dGgwfDU3NzRlNWUzNTQzNWU4N2Q3MjZjNTk5YiIsImF1ZCI6IkFabXRrQk41ekRHRVJKZXNGWkdGUzh2WUpZeVpUckRvIiwiZXhwIjoxNTg2MTA3MjQ1LCJpYXQiOjE0ODYwNzEyNDV9.su-bE4qCuLqoN83E3zsQW9rPSuBdW3HkWDbSgLKzfn4';
user_id='dev-test';
user_profile={nickname:'dev-test'};
user_log_in_event(user_profile);
```

## Raw database access

	docker exec -ti -u postgres openeth_dbm_1 psql -d openeth -P pager=off

## Dump database for backups

	docker exec -ti -u postgres openeth_dbm_1 pg_dump -a --insert openeth

## JWT token:

Example token (with signature removed):


```
eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJyb2xlIjoiYXV0aG9yIiwidXNlcmlkIjoiYXV0aDB8NTZkZWEwYjM4MWRlMjkyZTBjYjc1OTY1IiwiaXNzIjoiaHR0cHM6Ly9vcGVuZXRoLmF1dGgwLmNvbS8iLCJzdWIiOiJhdXRoMHw1NmRlYTBiMzgxZGUyOTJlMGNiNzU5NjUiLCJhdWQiOiJBWm10a0JONXpER0VSSmVzRlpHRlM4dllKWXlaVHJEbyIsImV4cCI6MTQ1NzQ4NjM5MywiaWF0IjoxNDU3NDUwMzkzfQ.2DIZz2bf19Jr9UaNA3DLl263JqzXvrAUky3Vr_ZgIbQ
```

```
{
	"role": "author",
	"userid": "auth0|56dea0b381de292e0cb75965",
	"iss": "https://openeth.auth0.com/",
	"sub": "auth0|56dea0b381de292e0cb75965",
	"aud": "AZmtkBN5zDGERJesFZGFS8vYJYyZTrDo",
	"exp": 1457486393,
	"iat": 1457450393
}
```

The `role` gets mapped to a PostgreSQL role, `sub` is used to uniquely identify
users.


## Regenerating Diffie-Hellman parameters

Goal:

* A+ on <https://www.ssllabs.com/ssltest/analyze.html?d=openeth.com>
* <https://cyh.herokuapp.com/cyh>

<https://www.owasp.org/index.php/List_of_useful_HTTP_headers>

<https://raymii.org/s/tutorials/Strong_SSL_Security_On_nginx.html>

	openssl dhparam -out certificates/dhparam.pem 4096


## Content Security Policy

* <https://www.w3.org/TR/CSP/>
* <https://developer.mozilla.org/en-US/docs/Web/Security/CSP/CSP_policy_directives>


## Stack:

* [Auth0](https://auth0.com/) as authentication provider.
* [Let’s Encrypt](https://letsencrypt.org/) as certificate authority.
* [Nginx](http://nginx.org/) as web server.
* [PostgREST](http://postgrest.com/) as API server.
* [Sqitch](http://sqitch.org/) for database migration.
* [PostgreSQL](http://www.postgresql.org/) as database engine.
* [Docker](https://www.docker.com/) to containerize.
* [Docker compose](https://docs.docker.com/compose/) for orchestrating containers.

