#!/usr/bin/env bash
# Workaround for https://github.com/opennextjs/opennextjs-cloudflare/issues:
# patchVercelOgLibrary does not mkdir destination parent before copyFileSync in
# pnpm monorepo setups, causing ENOENT during opennextjs-cloudflare build.
#
# This script injects mkdirSync before copyFileSync in the OpenNext runtime.
# Intended to be run before `opennextjs-cloudflare build`.
set -euo pipefail

target_glob="*/@opennextjs/cloudflare/dist/cli/build/patches/ast/patch-vercel-og-library.js"
search_root="${1:-.}"

patched_count=0
while IFS= read -r -d '' file; do
  if grep -q "mkdirSync" "$file"; then
    continue
  fi
  sed -i.bak 's|import { copyFileSync, existsSync, readFileSync, renameSync, writeFileSync } from "node:fs";|import { copyFileSync, existsSync, mkdirSync, readFileSync, renameSync, writeFileSync } from "node:fs";|' "$file"
  sed -i.bak 's|copyFileSync(tracedEdgePath, outputEdgePath);|mkdirSync(path.dirname(outputEdgePath), { recursive: true });\
            copyFileSync(tracedEdgePath, outputEdgePath);\
            const tracedFontPath = path.join(path.dirname(traceInfoPath), tracedNodePath.replace("index.node.js", "Geist-Regular.ttf"));\
            if (existsSync(tracedFontPath)) { copyFileSync(tracedFontPath, path.join(outputDir, "Geist-Regular.ttf")); }|' "$file"
  rm -f "${file}.bak"
  patched_count=$((patched_count + 1))
  echo "patched: $file"
done < <(find "$search_root" -path "$target_glob" -print0 2>/dev/null)

echo "patched $patched_count file(s)"
