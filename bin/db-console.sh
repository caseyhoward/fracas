#!/usr/bin/env sh

set -e

docker-compose up -d &&
docker-compose exec postgres psql -U fracas