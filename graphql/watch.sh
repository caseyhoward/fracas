#!/usr/bin/env sh

generate_elm()
{
  rm -rf src/Api
  npx elm-graphql --introspection-file graphql.schema.json --base Api
}

generate_typescript()
{
  npx graphql-codegen -- --config codegen.yml
}

generate_typescript && generate_elm

inotifywait -q -m -e close_write ./server/schema.graphql |
while read -r filename event; do
  generate_typescript && generate_elm
done