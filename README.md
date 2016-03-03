# OpenEth

## Dependencies

Make sure you have a recent version of `docker-compose`, at least version *1.6*.

	curl -L https://github.com/docker/compose/releases/download/1.6.2/docker-compose-`uname -s`-`uname -m` > /usr/local/bin/docker-compose
	chmod +x /usr/local/bin/docker-compose


## Starting

	docker-compose up

To start with a clean build

	rm -rf data log
	docker-compose rm 
	