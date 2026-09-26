FROM node:24.13.0-bookworm-slim@sha256:4660b1ca8b28d6d1906fd644abe34b2ed81d15434d26d845ef0aced307cf4b6f

ENV COREPACK_ENABLE_DOWNLOAD_PROMPT=0
ENV WRANGLER_SEND_METRICS=false

WORKDIR /workspace/applications/backend/media

COPY package.json pnpm-lock.yaml pnpm-workspace.yaml ./
RUN corepack enable \
    && corepack prepare pnpm@12.4.2 --activate \
    && pnpm install --frozen-lockfile

COPY tsconfig.json ./tsconfig.json
COPY migrations ./migrations
COPY runtime ./runtime
COPY test/feature ./test/feature

CMD ["node", "test/feature/run.mjs"]
