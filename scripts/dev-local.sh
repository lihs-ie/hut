#!/bin/bash

set -e

# 色付き出力
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# プロジェクトルートに移動
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
cd "$PROJECT_ROOT"

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}  Hut - Development Environment${NC}"
echo -e "${BLUE}========================================${NC}"

# Emulatorの状態確認
echo -e "\n${YELLOW}Checking Firebase Emulator...${NC}"

if curl -s http://localhost:8085 > /dev/null 2>&1; then
    echo -e "${GREEN}Firestore Emulator is already running${NC}"
else
    echo -e "${BLUE}Starting Firebase Emulator...${NC}"
    pnpm emulator:start:detach

    # Emulatorが起動するまで待機
    MAX_ATTEMPTS=30
    ATTEMPT=0

    while [ $ATTEMPT -lt $MAX_ATTEMPTS ]; do
        if curl -s http://localhost:8085 > /dev/null 2>&1; then
            echo -e "${GREEN}Firestore Emulator is ready!${NC}"
            break
        fi
        ATTEMPT=$((ATTEMPT + 1))
        echo -n "."
        sleep 1
    done

    if [ $ATTEMPT -eq $MAX_ATTEMPTS ]; then
        echo -e "\n${RED}Error: Firestore Emulator failed to start${NC}"
        exit 1
    fi
fi

# 開発サーバーの起動
echo -e "\n${YELLOW}Starting development servers...${NC}"
echo -e "${BLUE}  - Reader: http://localhost:3000${NC}"
echo -e "${BLUE}  - Admin:  http://localhost:3001${NC}"
echo -e "${BLUE}  - Emulator UI: http://localhost:4000${NC}"
echo ""

pnpm dev
