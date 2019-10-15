#!/usr/bin/env sh

# docker-compose exec postgres psql -U fracas -c 'create database "fracas";' || true
docker-compose exec postgres psql -U fracas -c 'create database "fracas_test";' || true

docker run --rm -v `pwd`/db/migrations:/flyway/sql flyway/flyway:6.0.4 -url=jdbc:postgresql://`ip addr show docker0 | grep -Po 'inet \K[\d.]+'`:5432/fracas -user=fracas -password=abc123 migrate
docker run --rm -v `pwd`/db/migrations:/flyway/sql flyway/flyway:6.0.4 -url=jdbc:postgresql://`ip addr show docker0 | grep -Po 'inet \K[\d.]+'`:5432/fracas_test -user=fracas -password=abc123 migrate