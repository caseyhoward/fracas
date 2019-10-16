#!/usr/bin/env sh


if [ -d client/src/Api ]; then
  mv client/src/Api client/src/Api.bak
fi

cd client && ( (elm-graphql http://localhost:4000 --base Api && rm -rf client/src/Api.bak) || (mv src/Api.bak src/Api && exit 1 ) ) && cd -
cd server && npm run graphql-codegen && cd -