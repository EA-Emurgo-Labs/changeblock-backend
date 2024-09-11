# Setup

Before running Changeblock Server we need to setup few Environment variables and load funds.

## 1. Setup Environment Variables

```shell
BLOCKFROST_IPFS= <Blockfrost IPFS KEY>
DB_CONNECTION=   <Postgresql Db Connection String>
MARKETPLACE_VERSION= <Marketplace Version to use. should be Hex encoded>

# We can set below variables after running creating Oracle & Deploy Marketplace script
ORACLE_UTXO_REF = <Output Reference of Oracle>
MARKETPLACE_REF_SCRIPT_UTXO=<Output Reference of deployed Marketpalce Reference Script>
```

## 2. Load Collateral UTXO & Fund to Root address

To view collateral address we can simply run following command and send exactly 5 ADA to that address.

```shell
  make show-internal-collateral-address
```

To view normal interal address we can run following command and send Some ADA to that address.

```shell
  make show-internal-address
```

## 3. Create Oracle

To create oracle we need to run

```
  cabal run elabs-backend:cli -- oracle create --rate <rate_in_lovelace>

```

We need to export variable displayed in output of terminal once createOracle script is completed.

## 4. Deploy Marketplace Script

we can simply run following command to deploy Marketplace script.

```
  cabal run elabs-backend:cli -- marketplace deploy --address <Address-to-send-ref-script>
```

Like Oracle we need to export variable displayed in output of terminal after script is completed.

