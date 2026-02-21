FROM node:20-slim AS deps

RUN corepack enable pnpm
WORKDIR /app

COPY package.json pnpm-workspace.yaml pnpm-lock.yaml ./
COPY shared/package.json ./shared/
COPY reader/package.json ./reader/
COPY admin/package.json ./admin/

# pnpm-workspace.yaml references ../scripts which is outside the Docker context.
# Create a stub package.json to satisfy workspace resolution.
RUN mkdir -p /scripts && \
    printf '{"name":"@hut/scripts","version":"0.0.0","private":true,"type":"module","dependencies":{"ulid":"^3.0.2"}}' > /scripts/package.json

RUN pnpm install --frozen-lockfile

FROM node:20-slim AS builder

RUN corepack enable pnpm
WORKDIR /app

ARG APP_NAME
ARG NEXT_PUBLIC_FIREBASE_PROJECT_ID
ARG NEXT_PUBLIC_FIREBASE_API_KEY=""
ARG NEXT_PUBLIC_FIREBASE_AUTH_DOMAIN=""
ARG NEXT_PUBLIC_FIREBASE_STORAGE_BUCKET=""
ARG NEXT_PUBLIC_FIREBASE_MESSAGING_SENDER_ID=""
ARG NEXT_PUBLIC_FIREBASE_APP_ID=""

COPY --from=deps /app ./
COPY . .

RUN pnpm --filter @hut/${APP_NAME} build

FROM node:20-slim AS runner

WORKDIR /app

RUN addgroup --system --gid 1001 nodejs && \
    adduser --system --uid 1001 nextjs

ENV NODE_ENV=production

ARG APP_NAME
ARG APP_PORT=3000
ENV APP_NAME=${APP_NAME}
ENV PORT=${APP_PORT}
ENV HOSTNAME="0.0.0.0"

COPY --from=builder --chown=nextjs:nodejs /app/${APP_NAME}/.next/standalone ./
COPY --from=builder --chown=nextjs:nodejs /app/${APP_NAME}/.next/static ./${APP_NAME}/.next/static
COPY --from=builder --chown=nextjs:nodejs /app/${APP_NAME}/public ./${APP_NAME}/public

USER nextjs

EXPOSE ${APP_PORT}

CMD ["sh", "-c", "node ${APP_NAME}/server.js"]
