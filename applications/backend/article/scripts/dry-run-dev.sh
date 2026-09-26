#!/usr/bin/env bash
set -euo pipefail

for worker in article-do-worker article-excerpt-worker article-completion-worker; do
  corepack pnpm@12.4.2 exec wrangler deploy \
    --config "runtime/${worker}/wrangler.jsonc" \
    --env dev \
    --dry-run \
    --x-provision=false \
    --x-auto-create=false
done
