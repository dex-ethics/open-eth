#!/bin/sh
docker-compose stop
sudo rm -rf data/data
docker-compose rm -f
docker-compose create
docker-compose start

# PostgREST fails on start if PostgreSQL is not up and running,  restart it
docker-compose stop api
docker-compose start api

