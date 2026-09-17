#!/usr/bin/env sh
set -eu

cd "$(dirname "$0")"
docker compose up --build --abort-on-container-exit --exit-code-from feature
