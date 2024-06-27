# Wallet backend
This documentation should help developers to understand how the internal wallet works.

In the current design, the root key is stored on disk and read into the program when started.
The root key is used to create new addresses and signing keys.

To create a root key, it's best to create a new wallet with your favorite Cardano wallet and then use the mnemonic to generate the root key for the server.

This command will create the `root.key`
```

 cabal run elabs-backend:cli -- wallet gen-root-key --mnemonic "brand scatter almost cattle reward guilt one sound embrace payment want brand april kiwi major novel orchard innocent interest sense alley deny main fit"
```

## Secret Zero Problem

The challenge of key managers is that, for example, in order to get the root key from a vault, you need an access key for the vault. The access key would be stored on the disc, too, so you only partially solve this issue.

Current services, like Hardware Security Modules (HSM) or Key Management Services (KMS), solve this issue by not letting the key never leave the service. It means that all cryptographic computations, like deriving addresses, signing transactions, etc., would be done within the HSM or KMS.

I could not find any trustful cloud-based HSM that supports our needed algorithms. An HSM solution would also require more research and time on our site.

A compromise would be using a key vault service to store the root key and an access token to access the service. This compromise gives more security in that the vault service could be restricted to who has access to the service, for example, restricted by IP. Also, such a service would offer a logging system, so we know when and who is accessing the service. Such a solution would also prevent us from saving the root key on the hard disc.

However, if the access token is stolen, the attacker could still access the service from within the server and get the root key.

Another attack vector is from within the application if the attacker has access to the memory space. In this case, the root key could be read from the memory.

There is no real improvement.

For the mainnet release, we should focus on using the cardano-wallet microservice.
The cardano-wallet would give us these benefits:
- We can restrict access to the cardano-wallet service via TLS, IP, etc.
- We can create a wallet via mnemonic and access it via passphrase
- Only the passphrase (not the root key) is revealed if our application server is compromised.
 - We can delete the wallet and create a new one with the same mnemonic but a different passphrase.

The cardano-wallet solution would offer us a lot of security with little effort.

## Wallet DB

We are using a relational database. Our schema is pretty simple, and it's found in the `Internal.Wallet.DB.Schema` module.

```
Account
  created UTCTime default=CURRENT_TIME
  deriving Show

Address
  accountId AccountId
  user UserId Maybe
  created UTCTime default=CURRENT_TIME
  deriving Show
```

The idea is that the `AccountId` and the `AddressId` derive a new address and payment key from the root key.

When creating the first server application, you must create at least one `Account` entry. With the `AccountId`, we can create the stake address and key to delegate all derived addresses. But I recommend doing this via a Cardano wallet.

To get all addresses from a user, we need to query the `Address` table by the `UserId` to get all related AccountId and AddressId pairs. We can derive all addresses and payment keys related to this user.

The `UserId` from the `Address` table is optional. This means that all addresses not belonging to any user can be used for internal purposes, such as collateral.
