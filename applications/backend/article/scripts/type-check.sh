#!/usr/bin/env bash
set -euo pipefail

for worker in article-do-worker article-excerpt-worker article-completion-worker; do
  corepack pnpm@12.4.2 exec tsc \
    --project "runtime/${worker}/tsconfig.json"
done
