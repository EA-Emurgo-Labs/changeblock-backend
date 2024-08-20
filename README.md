# EMURGO Labs Backend

You must first be in the nix development shell.
```
nix develop
```

This will create the `root.key`
```
 cabal run elabs-backend:cli -- wallet gen-root-key --mnemonic "brand scatter almost cattle reward guilt one sound embrace payment want brand april kiwi major novel orchard innocent interest sense alley deny main fit"

```

>[!NOTE]
> To Run Blockchain specific CLI command we need to setup `CORE_CONFIG_PATH` variable which points to config.json file.

This will create a test auth token:
```
cabal run elabs-backend:cli -- auth create --token TEST --notes Test
```

To withdraw carbon token: 
```bash
cabal run elabs-backend:cli  -- marketplace withdraw --carbon-policy-id <carbon policy ID>  --carbon-token-name <carbon token name> --out-address <receiver address> --qty <amount to withdraw> --backdoor-key-path /path/to/backdoor.skey --token-owner <carbon token owner pubkey hash>
```

## run api
```
cabal run elabs-backend:app run
```

## DB Backend
At the moment we are using PostgreSQL, the database must be a relational database
with rollback functionality.

## Integration Testing with local Node
 * Setup

    We need `cardano-cli  v8.1.1` to be available in our system for running privnet Test. Executable can be downloaded from [here](https://github.com/IntersectMBO/cardano-node/releases/download/8.1.1/cardano-node-8.1.1-linux.tar.gz)

To run the integration test, please ensure that `cardano-cli` and `cardano-node` are available in your environment. Also, you must be in the nix development environment (`nix develop`) before running the integration test with `make integration-test`.

> [!NOTE]
> After cardano-cli is available through flake manual setup of cardano-cli is not needed, appropriate version of cardano-cli will be automatically available.


## Other documents
- [Backend developer guide](docs/dev-guide.md)
- [Backdoor Setup](docs/backdoor.md)
- [Wallet backend](docs/wallet-backend.md)
- [Authorization header](docs/auth.md)
- [Setup Variables](docs/setup.md)
- [Deployig a docker image](docs/docker.md)
- [Infrastructure](docs/aws.md)
- [ChangeBlock related notes](docs/changeblock.md)