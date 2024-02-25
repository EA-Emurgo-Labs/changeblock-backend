# EMURGO Labs Backend

You must first be in the nix development shell.
```
nix develop
```

This will create the `root.key`
```
cabal run elabs-backend:app -- genrootkey --mnemonic "ripple scissors kick mammal hire column oak again sun offer wealth tomorrow wagon turn fatal"
```
This will create a test auth token:
```
cabal run elabs-backend:app -- addtoken --token TEST --notes Test
```

## run api
```
cabal run elabs-backend:app run
```

## DB Backend
At the moment we are using SQLite, the database must be a relational database
with rollback functionality.

## Integration Testing with local Node
 * Setup

    We need `cardano-cli  v8.1.1` to be available in our system for running privnet Test. Executable can be downloaded from [here](https://github.com/IntersectMBO/cardano-node/releases/download/8.1.1/cardano-node-8.1.1-linux.tar.gz)

To run the integration test, please ensure that `cardano-cli` and `cardano-node` are available in your environment. Also, you must be in the nix development environment (`nix develop`) before running the integration test with `make integration-test`.

> [!NOTE]
> After cardano-cli is available through flake manual setup of cardano-cli is not needed, appropriate version of cardano-cli will be automatically available.


## Other documents
- [Backend developer guide](docs/dev-guide.md)
- [Wallet backend](docs/wallet-backend.md)
- [Authorization header](docs/auth.md)
- [Setting up Changeblock Backend with Docker](docs/docker.md)