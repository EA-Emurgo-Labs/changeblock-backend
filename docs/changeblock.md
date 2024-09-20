# ChangeBlock related notes

The smart contract of Nexchange caused issues with the backend wallet due to
its unconventional design. As a result, we have limited the use of the backend
wallet.

* We modified the wallet backend to ensure that every user has exactly one
  unique address assigned to them.

* We have added a user lookup table for pubkey hashes. Every time we derive user
  keys, we ensure to store a public key to user ID mapping in the table.