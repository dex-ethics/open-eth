#!/bin/sh
docker-compose stop
sudo rm -rf data/data
docker-compose rm -f
docker-compose create
docker-compose start

