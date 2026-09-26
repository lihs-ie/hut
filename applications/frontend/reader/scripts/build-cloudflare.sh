#!/usr/bin/env bash
# Build the reader app for Cloudflare Workers via OpenNext.
#
# Node.js middleware (proxy.ts) is not yet supported by @opennextjs/cloudflare,
# so proxy.ts is moved aside during the Cloudflare build and restored afterwards.
# The response-header cleanup that proxy.ts performs on Cloud Run will be
# re-implemented at the Cloudflare Workers layer in a follow-up PR.
set -euo pipefail

proxy_file="src/proxy.ts"
proxy_backup="src/proxy.ts.cloudflare-disabled"

restore_proxy() {
  if [ -f "$proxy_backup" ]; then
    mv "$proxy_backup" "$proxy_file"
  fi
}
trap restore_proxy EXIT

bash scripts/patch-opennext.sh ..

if [ -f "$proxy_file" ]; then
  mv "$proxy_file" "$proxy_backup"
fi

BUILD_TARGET=cloudflare next build
ln -sfn applications/frontend/reader/.next .next/standalone/.next
opennextjs-cloudflare build --skipNextBuild
