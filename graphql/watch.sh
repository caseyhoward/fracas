#!/usr/bin/env sh

# This must be ran through docker-compose otherwise files won't be in the right place

./generate.sh

inotifywait -q -m -e close_write ./server/schema.graphql |
while read -r filename event; do
  ./generate.sh
done