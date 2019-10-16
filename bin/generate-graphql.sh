#!/usr/bin/env sh

# # It sucks losing your generated elm code when there server isn't running or isn't compiling
# if [ -d client/src/Api ]; then
#   rm -rf client/src/Api.bak
#   mv client/src/Api client/src/Api.bak
# fi

# cd client && ( (elm-graphql http://localhost:4000 --base Api && rm -rf client/src/Api.bak) || (mv src/Api.bak src/Api && exit 1 ) ) && cd -
cd client && bin/generate-graphql.sh && cd -
cd server && bin/generate-graphql.sh