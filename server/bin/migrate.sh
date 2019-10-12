#!/usr/bin/env sh

# createdb -h localhost -U fracas -p 5432 fracas || true
# createdb -h localhost -U fracas -p 5432 fracas-test || true

docker run --rm -v `pwd`/db/migrations:/flyway/sql flyway/flyway:6.0.4 -url=jdbc:postgresql://`ip addr show docker0 | grep -Po 'inet \K[\d.]+'`:5432/fracas -user=fracas -password=abc123 migrate
docker run --rm -v `pwd`/db/migrations:/flyway/sql flyway/flyway:6.0.4 -url=jdbc:postgresql://`ip addr show docker0 | grep -Po 'inet \K[\d.]+'`:5432/fracas-test -user=fracas -password=abc123 migrate