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
ARG NEXT_PUBLIC_SENTRY_DSN_READER=""
ARG NEXT_PUBLIC_SENTRY_DSN_ADMIN=""
ARG NEXT_PUBLIC_GIT_SHA=""
ARG SENTRY_ORG=""
ARG SENTRY_PROJECT=""

ENV NEXT_PUBLIC_FIREBASE_PROJECT_ID=${NEXT_PUBLIC_FIREBASE_PROJECT_ID}
ENV NEXT_PUBLIC_FIREBASE_API_KEY=${NEXT_PUBLIC_FIREBASE_API_KEY}
ENV NEXT_PUBLIC_FIREBASE_AUTH_DOMAIN=${NEXT_PUBLIC_FIREBASE_AUTH_DOMAIN}
ENV NEXT_PUBLIC_FIREBASE_STORAGE_BUCKET=${NEXT_PUBLIC_FIREBASE_STORAGE_BUCKET}
ENV NEXT_PUBLIC_FIREBASE_MESSAGING_SENDER_ID=${NEXT_PUBLIC_FIREBASE_MESSAGING_SENDER_ID}
ENV NEXT_PUBLIC_FIREBASE_APP_ID=${NEXT_PUBLIC_FIREBASE_APP_ID}
ENV SERVER_ACTIONS_ALLOWED_ORIGINS=${SERVER_ACTIONS_ALLOWED_ORIGINS}
ENV IMAGE_REMOTE_PATTERNS=${IMAGE_REMOTE_PATTERNS}
ENV NEXT_PUBLIC_SENTRY_DSN_READER=${NEXT_PUBLIC_SENTRY_DSN_READER}
ENV NEXT_PUBLIC_SENTRY_DSN_ADMIN=${NEXT_PUBLIC_SENTRY_DSN_ADMIN}
ENV NEXT_PUBLIC_GIT_SHA=${NEXT_PUBLIC_GIT_SHA}
# SENTRY_ORG / SENTRY_PROJECT identify the Sentry tenant and project at build
# time for source map upload via withSentryConfig. They are scoped to this
# stage and intentionally not propagated to the runtime image.
ENV SENTRY_ORG=${SENTRY_ORG}
ENV SENTRY_PROJECT=${SENTRY_PROJECT}

COPY . .

# Restore workspace fix (COPY overwrites pnpm-workspace.yaml with original)
RUN printf 'packages:\n  - "shared"\n  - "reader"\n  - "admin"\n' > pnpm-workspace.yaml

RUN find ${APP_NAME}/public -type l -delete 2>/dev/null; \
    cp -rn shared/public/. ${APP_NAME}/public/

# SENTRY_AUTH_TOKEN is mounted as a BuildKit secret so it is only available
# during the build RUN step and never written into any image layer. If the
# secret is not provided (e.g. local development), the file simply does not
# exist and the Sentry webpack plugin skips the source map upload.
RUN --mount=type=secret,id=gcp_credentials,target=/tmp/gcp-credentials.json \
    --mount=type=secret,id=gcp_oidc_token,target=/tmp/gcp-oidc-token \
    --mount=type=secret,id=sentry_auth_token,target=/tmp/sentry-auth-token \
    GOOGLE_APPLICATION_CREDENTIALS=/tmp/gcp-credentials.json \
    SENTRY_AUTH_TOKEN="$([ -f /tmp/sentry-auth-token ] && cat /tmp/sentry-auth-token || true)" \
    pnpm --filter @hut/${APP_NAME} build

FROM node:22-slim AS runner

WORKDIR /app

RUN addgroup --system --gid 1001 nodejs && \
    adduser --system --uid 1001 nextjs

ENV NODE_ENV=production

ARG APP_NAME
ARG APP_PORT=3000
ARG NEXT_PUBLIC_SENTRY_DSN_READER=""
ARG NEXT_PUBLIC_SENTRY_DSN_ADMIN=""
ARG NEXT_PUBLIC_GIT_SHA=""
ENV APP_NAME=${APP_NAME}
ENV PORT=${APP_PORT}
ENV HOSTNAME="0.0.0.0"

# Runtime Sentry configuration. The server-side Sentry SDK reads the DSN and
# release tag at process start; these values are non-sensitive (public DSNs and
# the git SHA). SENTRY_AUTH_TOKEN is deliberately excluded from the runtime
# image — source map upload happens only at build time.
ENV NEXT_PUBLIC_SENTRY_DSN_READER=${NEXT_PUBLIC_SENTRY_DSN_READER}
ENV NEXT_PUBLIC_SENTRY_DSN_ADMIN=${NEXT_PUBLIC_SENTRY_DSN_ADMIN}
ENV NEXT_PUBLIC_GIT_SHA=${NEXT_PUBLIC_GIT_SHA}

# standalone output nests files under app/ due to turbopack.root ("../..")
COPY --from=builder --chown=nextjs:nodejs /app/${APP_NAME}/.next/standalone/app ./
COPY --from=builder --chown=nextjs:nodejs /app/${APP_NAME}/.next/static ./${APP_NAME}/.next/static
COPY --from=builder --chown=nextjs:nodejs /app/${APP_NAME}/public ./${APP_NAME}/public

USER nextjs

EXPOSE ${APP_PORT}

CMD ["sh", "-c", "node ${APP_NAME}/server.js"]
