#!/usr/bin/env sh

rm -rf src/Api

./server/bin/generate-graphql.sh && ./client/bin/generate-graphql.sh

inotifywait -q -m -e close_write ./server/schema.graphql |
while read -r filename event; do
  ./server/bin/generate-graphql.sh && ./client/bin/generate-graphql.sh
done