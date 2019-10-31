#!/usr/bin/env sh

set -e

# cd client && bin/build.sh && cd - && cd server && bin/build.sh

docker-compose up -d --build client server postgres postgres-migration-development postgres-migration-test
docker-compose exec server bin/build.sh
docker-compose exec client bin/build.sh
docker-compose stop