## Backdoor
To upgrade Marketplace contract we introduced Backdoor which bypass additional checks if Transaction is signed by  Backdoor Address.

### Setup 

1. create Signing & Verification key

```bash
  cardano-cli address key-gen \
    --verification-key-file /path/to/backdoor.vkey \
    --signing-key-file /path/to/backdoor.skey
```


2. Create Backdoor Address
```bash
  cardano-cli address build \
    --payment-verification-key-file /path/to/backdoor.vkey \
    --out-file /path/to/backdoor.addr \
    --testnet-magic 1
```

This Backdoor address is used in Backend and further used to setup Marketplace contract.



### Backdoor Operation

1. Withdraw Carbon token 
```bash
  cabal run elabs-backend:cli -- marketplace withdraw  \
   --carbon-policy-id <carbon policy ID>  \
   --carbon-token-name <carbon token name> \
   --out-address <receiver address> \
   --qty <amount to withdraw> \
   --backdoor-key-path /path/to/backdoor.skey
```