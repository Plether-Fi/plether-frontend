FROM haskell:9.4.8-slim AS builder

RUN apt-get update && apt-get install -y --no-install-recommends \
    libpq-dev pkg-config && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /build

COPY apps/backend/plether-api.cabal apps/backend/cabal.project apps/backend/cabal.project.freeze ./
RUN cabal update && cabal build --only-dependencies

COPY apps/backend/app/ app/
COPY apps/backend/src/ src/
COPY apps/backend/test/ test/
RUN cabal build exe:plether-api && \
    cp "$(cabal list-bin plether-api)" /build/plether-api-bin

FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y --no-install-recommends \
    libpq5 ca-certificates && \
    rm -rf /var/lib/apt/lists/*

COPY --from=builder /build/plether-api-bin /usr/local/bin/plether-api
COPY apps/frontend/src/contracts/addresses.sepolia.json /app/config/addresses.sepolia.json
COPY apps/frontend/src/contracts/addresses.mainnet.json /app/config/addresses.mainnet.json

WORKDIR /app
EXPOSE 3001

CMD ["plether-api", "+RTS", "-N", "-RTS"]
