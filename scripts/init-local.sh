#!/bin/bash

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
cd "$PROJECT_ROOT"

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}  Hut - Local Init${NC}"
echo -e "${BLUE}========================================${NC}"

echo -e "\n${YELLOW}[1/4] Checking Docker...${NC}"

if ! command -v docker &> /dev/null; then
    echo -e "${RED}Error: Docker is not installed${NC}"
    exit 1
fi

if ! docker info &> /dev/null; then
    echo -e "${RED}Error: Docker daemon is not running${NC}"
    exit 1
fi

echo -e "${GREEN}Docker is ready${NC}"

echo -e "\n${YELLOW}[2/4] Starting Firebase Emulator...${NC}"

if curl -s http://localhost:8085 > /dev/null 2>&1; then
    echo -e "${GREEN}Firestore Emulator is already running${NC}"
else
    pnpm emulator:stop 2>/dev/null || true
    pnpm emulator:start:detach

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
fi

echo -e "\n${YELLOW}[3/4] Seeding test data...${NC}"
pnpm seed

echo -e "\n${YELLOW}[4/4] Starting development servers...${NC}"
echo -e "${BLUE}  - Reader:       http://localhost:3000${NC}"
echo -e "${BLUE}  - Admin:        http://localhost:3001${NC}"
echo -e "${BLUE}  - Emulator UI:  http://localhost:4000${NC}"
echo ""

cd applications/frontend && pnpm dev
