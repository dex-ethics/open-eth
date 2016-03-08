# OpenEth

Stack:

* [Auth0](https://auth0.com/) as authentication provider.
* [Let’s Encrypt](https://letsencrypt.org/) as certificate authority.
* [Nginx](http://nginx.org/) as web server.
* [PostgREST](http://postgrest.com/) as API server.
* [Sqitch](http://sqitch.org/) for database migration.
* [PostgreSQL](http://www.postgresql.org/) as database engine.
* [Docker](https://www.docker.com/) to containerize.
* [Docker compose](https://docs.docker.com/compose/) for orchestrating containers.

## Dependencies

Make sure you have a recent version of `docker-compose`, at least version *1.6*.

	curl -L https://github.com/docker/compose/releases/download/1.6.2/docker-compose-`uname -s`-`uname -m` > /usr/local/bin/docker-compose
	chmod +x /usr/local/bin/docker-compose

## Starting

	docker-compose up

To start with a clean build

	sudo rm -rf data/data
	docker-compose rm -f


## JWT token:

Example token (with signature removed):

```
eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJyb2xlIjoiYXV0aG9yIiwiaXNzIjoiaHR0cHM6Ly9vcGVuZXRoLmF1dGgwLmNvbS8iLCJzdWIiOiJhdXRoMHw1NmRlYTBiMzgxZGUyOTJlMGNiNzU5NjUiLCJhdWQiOiJBWm10a0JONXpER0VSSmVzRlpHRlM4dllKWXlaVHJEbyIsImV4cCI6MTQ1NzQ2OTQwNSwiaWF0IjoxNDU3NDMzNDA1fQ
```

```
{
  "role": "author",
  "iss": "https://openeth.auth0.com/",
  "sub": "auth0|56dea0b381de292e0cb75965",
  "aud": "AZmtkBN5zDGERJesFZGFS8vYJYyZTrDo",
  "exp": 1457469405,
  "iat": 1457433405
}
```

The `role` gets mapped to a PostgreSQL role.

