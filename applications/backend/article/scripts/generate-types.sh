#!/usr/bin/env bash
set -euo pipefail

for worker in article-do-worker article-excerpt-worker article-completion-worker; do
  corepack pnpm@12.4.2 exec wrangler types \
    "runtime/${worker}/generated/worker-configuration.d.ts" \
    --config "runtime/${worker}/wrangler.jsonc" \
    --env dev
  perl -pi -e 's/[ \t]+$//' \
    "runtime/${worker}/generated/worker-configuration.d.ts"
done
