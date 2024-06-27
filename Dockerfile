FROM nixos/nix:latest as builder

LABEL name=elabs-backend

ARG CACHIX_AUTHTOKEN
RUN echo "experimental-features = nix-command flakes" >> /etc/nix/nix.conf
RUN nix-env -iA nixpkgs.cachix
RUN cachix authtoken ${CACHIX_AUTHTOKEN}
RUN cachix use ea-emurgo-labs

COPY . /tmp/build
WORKDIR /tmp/build

RUN nix build .#elabs-backend:exe:app --accept-flake-config

RUN mkdir /tmp/nix-store-closure
RUN cp -R $(nix-store -qR result/) /tmp/nix-store-closure

#FROM scratch
FROM nixos/nix:latest

WORKDIR /app

COPY --from=builder /tmp/nix-store-closure /nix/store
COPY --from=builder /tmp/build/result /app

COPY ./contracts /app/contracts
COPY ./config.json /app
COPY ./.env /app
COPY ./root.key /app
ADD swagger-ui /app/swagger-ui

EXPOSE 8081/tcp

STOPSIGNAL SIGINT

ENTRYPOINT ["/app/bin/app", "run", "--port", "8081"]

