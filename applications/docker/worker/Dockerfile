FROM haskell:9.12.2-bookworm

RUN apt-get update && apt-get install -y --no-install-recommends \
  curl \
  ca-certificates \
  pkg-config \
  libgmp-dev \
  zlib1g-dev \
  && rm -rf /var/lib/apt/lists/*

RUN cabal update

WORKDIR /var/www/app

EXPOSE 8080
