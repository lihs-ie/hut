FROM node:22-slim AS deps

RUN corepack enable pnpm
WORKDIR /app

COPY package.json pnpm-workspace.yaml pnpm-lock.yaml ./
COPY shared/package.json ./shared/
COPY reader/package.json ./reader/
COPY admin/package.json ./admin/

# pnpm-workspace.yaml references ../scripts which is outside the Docker context.
# Override to exclude it; pnpm tolerates the extra importer in the lockfile.
RUN printf 'packages:\n  - "shared"\n  - "reader"\n  - "admin"\n' > pnpm-workspace.yaml

RUN pnpm install --frozen-lockfile

FROM deps AS builder

ARG APP_NAME
ARG NEXT_PUBLIC_FIREBASE_PROJECT_ID
ARG NEXT_PUBLIC_FIREBASE_API_KEY=""
ARG NEXT_PUBLIC_FIREBASE_AUTH_DOMAIN=""
ARG NEXT_PUBLIC_FIREBASE_STORAGE_BUCKET=""
ARG NEXT_PUBLIC_FIREBASE_MESSAGING_SENDER_ID=""
ARG NEXT_PUBLIC_FIREBASE_APP_ID=""
ARG SERVER_ACTIONS_ALLOWED_ORIGINS=""
ARG IMAGE_REMOTE_PATTERNS=""

ENV NEXT_PUBLIC_FIREBASE_PROJECT_ID=${NEXT_PUBLIC_FIREBASE_PROJECT_ID}
ENV NEXT_PUBLIC_FIREBASE_API_KEY=${NEXT_PUBLIC_FIREBASE_API_KEY}
ENV NEXT_PUBLIC_FIREBASE_AUTH_DOMAIN=${NEXT_PUBLIC_FIREBASE_AUTH_DOMAIN}
ENV NEXT_PUBLIC_FIREBASE_STORAGE_BUCKET=${NEXT_PUBLIC_FIREBASE_STORAGE_BUCKET}
ENV NEXT_PUBLIC_FIREBASE_MESSAGING_SENDER_ID=${NEXT_PUBLIC_FIREBASE_MESSAGING_SENDER_ID}
ENV NEXT_PUBLIC_FIREBASE_APP_ID=${NEXT_PUBLIC_FIREBASE_APP_ID}
ENV SERVER_ACTIONS_ALLOWED_ORIGINS=${SERVER_ACTIONS_ALLOWED_ORIGINS}
ENV IMAGE_REMOTE_PATTERNS=${IMAGE_REMOTE_PATTERNS}

COPY . .

# Restore workspace fix (COPY overwrites pnpm-workspace.yaml with original)
RUN printf 'packages:\n  - "shared"\n  - "reader"\n  - "admin"\n' > pnpm-workspace.yaml

RUN pnpm --filter @hut/${APP_NAME} build

FROM node:22-slim AS runner

WORKDIR /app

RUN addgroup --system --gid 1001 nodejs && \
    adduser --system --uid 1001 nextjs

ENV NODE_ENV=production

ARG APP_NAME
ARG APP_PORT=3000
ENV APP_NAME=${APP_NAME}
ENV PORT=${APP_PORT}
ENV HOSTNAME="0.0.0.0"

# standalone output nests files under app/ due to turbopack.root ("../..")
COPY --from=builder --chown=nextjs:nodejs /app/${APP_NAME}/.next/standalone/app ./
COPY --from=builder --chown=nextjs:nodejs /app/${APP_NAME}/.next/static ./${APP_NAME}/.next/static
COPY --from=builder --chown=nextjs:nodejs /app/${APP_NAME}/public ./${APP_NAME}/public

USER nextjs

EXPOSE ${APP_PORT}

CMD ["sh", "-c", "node ${APP_NAME}/server.js"]
