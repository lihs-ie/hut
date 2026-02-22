FROM haskell:9.12.2-bookworm AS builder

RUN apt-get update && \
    apt-get install -y --no-install-recommends pkg-config libz-dev && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /build

COPY search-token-worker.cabal cabal.project cabal.project.freeze ./

RUN cabal update && cabal build --only-dependencies

COPY . .

RUN cabal build exe:app && \
    cp "$(cabal list-bin app)" /opt/app

FROM debian:bookworm-slim AS runner

RUN apt-get update && \
    apt-get install -y --no-install-recommends ca-certificates && \
    rm -rf /var/lib/apt/lists/* && \
    addgroup --system app && \
    adduser --system --ingroup app app

COPY --from=builder /opt/app /usr/local/bin/app

USER app

EXPOSE 8080

ENV PORT=8080

CMD ["app"]
