module EA.CLI.Wallet.Parser (
  parseWalletCommand,
) where

import EA.CLI.Wallet.Command (
  WalletCommand (..),
  WalletGenRootKeyCommand (WalletGenRootKeyCommand),
  WalletInternalAddressCommand (WalletInternalAddressCommand),
 )
import Options.Applicative (
  Parser,
  command,
  help,
  hsubparser,
  info,
  long,
  metavar,
  progDesc,
  showDefault,
  strOption,
  switch,
  value,
 )

parseWalletCommand :: Parser WalletCommand
parseWalletCommand =
  hsubparser
    ( command
        "internal-address"
        (info (WltHandleInternalAddress <$> pWalletInternalAddressCommand) (progDesc "Handle internal address"))
        <> command
          "gen-root-key"
          (info (WltGenRootKey <$> pWalletGenRootKeyCommand) (progDesc "Generate root key"))
    )

pWalletInternalAddressCommand :: Parser WalletInternalAddressCommand
pWalletInternalAddressCommand =
  WalletInternalAddressCommand
    <$> switch
      ( long "collateral"
          <> help "Generate a collateral address"
      )

pWalletGenRootKeyCommand :: Parser WalletGenRootKeyCommand
pWalletGenRootKeyCommand =
  WalletGenRootKeyCommand
    <$> strOption
      ( long "mnemonic"
          <> metavar "MNEMONIC"
          <> help "Mnemonic for root key generation (24 words)"
      )
    <*> strOption
      ( long "path"
          <> metavar "PATH"
          <> help "Path to write root key"
          <> showDefault
          <> value "root.key"
      )