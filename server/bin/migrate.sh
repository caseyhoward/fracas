#!/usr/bin/env sh

docker run --rm -v `pwd`/db/migrations:/flyway/sql flyway/flyway:6.0.4 -url=jdbc:postgresql://`ip addr show docker0 | grep -Po 'inet \K[\d.]+'`:5432/fracas -user=fracas -password=abc123 migrate