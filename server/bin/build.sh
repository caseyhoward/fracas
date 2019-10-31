#!/usr/bin/env sh

set -e

docker-compose up -d postgres-migration-development
docker-compose up -d postgres-migration-test
# docker-compose up -d 
# npm install
# npm run test
