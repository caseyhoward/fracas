#!/usr/bin/env sh

set -e

docker-compose exec postgres psql -U fracas -c 'create database "fracas_test";' || true &&
docker run --rm -v `pwd`/db/migrations:/flyway/sql flyway/flyway:6.0.4 -url=jdbc:postgresql://`ip addr show docker0 | grep -Po 'inet \K[\d.]+'`:5432/fracas_test -user=fracas -password=abc123 migrate &&
npm install
npm run test
