#!/usr/bin/env sh

set -e

cd client && bin/build.sh && cd - && cd server && bin/build.sh
