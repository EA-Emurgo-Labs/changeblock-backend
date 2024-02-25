FROM nixos/nix:latest as build

ARG CACHIX_AUTHTOKEN

RUN echo "experimental-features = nix-command flakes" >> /etc/nix/nix.conf

RUN nix-env -iA cachix -f https://cachix.org/api/v1/install

RUN cachix authtoken ${CACHIX_AUTHTOKEN}

RUN cachix use ea-emurgo-labs

LABEL name=elabs-backend

WORKDIR /app

COPY . /app/

RUN nix develop --accept-flake-config

EXPOSE 8081/tcp

STOPSIGNAL SIGINT

ENTRYPOINT ["/app/bin/app", "run", "--port", "8081"]

