#!/usr/bin/env sh

nodemon --watch '**/*.ts' --watch 'schema.graphql' --ignore 'src/**/*.spec.ts' --exec 'ts-node' index.ts