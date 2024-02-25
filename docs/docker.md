# Setting up Changeblock Backend with Docker

This guide will walk you through the process of setting up the Changeblock backend using Docker.

## Step 1: Clone the Repository

First, clone the Changeblock backend source code to a new directory using the following command:

```bash
git clone --depth 1 git@github.com:EA-Emurgo-Labs/changeblock-backend.git changeblock-backend-docker
```

## Step 2: Configure the Application

Next, copy the `config.example.json` file and rename it to `config.json`. Open this file and fill out the core provider information.

Similarly, copy the `.env.example` file and rename it to `.env`. Replace the `BLOCKFROST_IPFS` token in this file with your actual token.

## Step 3: Create a New Wallet

Use your wallet app to create a new wallet. Write down the mnemonic and use it to create the root key for the backend:

```bash
cabal run elabs-backend:app -- genrootkey --mnemonic "<MNEMONIC>"
```

## Step 4: Build the Binary

At the moment, there is an issue with building the binary within Docker. As a workaround, you can prebuild the binary yourself:

```bash
nix develop --accept-flake-config
cabal install elabs-backend:app --installdir=bin
```

## Step 5: Build the Docker Image

Finally, obtain a Cachix authentication token and use it to build the Docker image:

```bash
docker build --build-arg CACHIX_AUTHTOKEN=<AUTHTOKEN> -t elabs-backend .
```

That's it! You have now successfully set up the Changeblock backend using Docker.