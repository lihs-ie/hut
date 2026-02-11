FROM node:24.13.0-slim

RUN apt-get update && apt-get install -y \
    wget \
    curl \
    && rm -rf /var/lib/apt/lists/*

RUN mkdir -p /etc/apt/keyrings && \
    wget -O - https://packages.adoptium.net/artifactory/api/gpg/key/public | tee /etc/apt/keyrings/adoptium.asc && \
    echo "deb [signed-by=/etc/apt/keyrings/adoptium.asc] https://packages.adoptium.net/artifactory/deb $(awk -F= '/^VERSION_CODENAME/{print$2}' /etc/os-release) main" | tee /etc/apt/sources.list.d/adoptium.list && \
    apt-get update && \
    apt-get install -y temurin-21-jre && \
    rm -rf /var/lib/apt/lists/*

ENV PNPM_HOME="/root/.local/share/pnpm"
ENV PATH="$PNPM_HOME:$PATH"
RUN corepack enable && corepack prepare pnpm@latest --activate
RUN pnpm add -g firebase-tools@latest

WORKDIR /app

COPY firebase.json .firebaserc firestore.rules storage.rules ./
COPY functions/ ./functions/

RUN mkdir -p /app/data/firestore /app/data/storage /app/data/auth

COPY scripts/entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

EXPOSE 4000 8085 9099 9199 5001 9299

ENTRYPOINT ["/entrypoint.sh"]
