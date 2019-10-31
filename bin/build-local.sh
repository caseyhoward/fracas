#!/usr/bin/env sh

set -e

# TODO: Make this also generate graphql code

docker-compose up -d client server postgres postgres-migration-development postgres-migration-test
docker-compose exec server bin/build.sh
docker-compose exec client bin/build.sh