#!/usr/bin/env sh

set -e

cd server && docker-compose up -d --build
