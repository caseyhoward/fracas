#!/usr/bin/env sh

rm -rf src/Api
elm-graphql http://localhost:4000 --base Api