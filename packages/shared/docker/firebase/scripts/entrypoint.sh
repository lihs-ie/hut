#!/bin/bash
set -e

# Functions の依存関係をインストール（volumeマウント後に実行される）
cd /app/functions
if [ ! -d "node_modules" ] || [ -z "$(ls -A node_modules 2>/dev/null)" ]; then
    echo "Installing functions dependencies..."
    pnpm install
fi
if [ ! -d "lib" ] || [ -z "$(ls -A lib 2>/dev/null)" ]; then
    echo "Building functions..."
    pnpm run build
fi
cd /app

EXPORT_PATH="/app/data"
IMPORT_FLAGS=""

if [ -d "$EXPORT_PATH/firestore" ] && [ "$(ls -A $EXPORT_PATH/firestore 2>/dev/null)" ]; then
    IMPORT_FLAGS="--import=$EXPORT_PATH"
fi

exec firebase emulators:start \
    --project=demo-hut \
    --only auth,firestore,storage,functions,eventarc \
    $IMPORT_FLAGS \
    --export-on-exit=$EXPORT_PATH
