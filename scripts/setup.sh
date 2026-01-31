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
echo -e "${BLUE}  Hut - Local Environment Setup${NC}"
echo -e "${BLUE}========================================${NC}"

# 依存関係のインストール
echo -e "\n${YELLOW}[1/4] Installing dependencies...${NC}"
pnpm install

# Docker確認
echo -e "\n${YELLOW}[2/4] Checking Docker...${NC}"
if ! command -v docker &> /dev/null; then
    echo -e "${RED}Error: Docker is not installed${NC}"
    exit 1
fi

if ! docker info &> /dev/null; then
    echo -e "${RED}Error: Docker daemon is not running${NC}"
    exit 1
fi

echo -e "${GREEN}Docker is ready${NC}"

# Firebase Emulatorの起動
echo -e "\n${YELLOW}[3/4] Starting Firebase Emulator...${NC}"

# 既存のコンテナを停止
pnpm emulator:stop 2>/dev/null || true

# Emulatorをバックグラウンドで起動
pnpm emulator:start:detach

# Emulatorが起動するまで待機
echo -e "${BLUE}Waiting for Firestore Emulator to be ready...${NC}"
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
    pnpm emulator:logs
    exit 1
fi

# シードデータの投入
echo -e "\n${YELLOW}[4/4] Seeding test data...${NC}"
cd packages/shared && pnpm seed && cd ../..

echo -e "\n${GREEN}========================================${NC}"
echo -e "${GREEN}  Setup completed successfully!${NC}"
echo -e "${GREEN}========================================${NC}"

echo -e "\n${BLUE}Available services:${NC}"
echo -e "  - Firestore Emulator: http://localhost:8085"
echo -e "  - Emulator UI:        http://localhost:4000"

echo -e "\n${BLUE}Next steps:${NC}"
echo -e "  - Run ${YELLOW}pnpm dev${NC} to start the development server"
echo -e "  - Run ${YELLOW}pnpm dev:reader${NC} to start only the reader app"
echo -e "  - Run ${YELLOW}pnpm dev:admin${NC} to start only the admin app"
echo -e "  - Run ${YELLOW}pnpm emulator:stop${NC} to stop the emulator"
