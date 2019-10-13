#!/usr/bin/env sh

rm -rf client/src/Api
cd client && elm-graphql http://localhost:4000 --base Api && cd -
cd server && npm run graphql-codegen && cd -