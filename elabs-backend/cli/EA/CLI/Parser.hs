module EA.CLI.Parser (
  parseRootCommand,
) where

import EA.CLI.Auth.Parser (parseAuthCommand)
import EA.CLI.Marketplace.Parser (parseMarketplaceCommand)
import EA.CLI.Oracle.Parser (parseOracleCommand)
import EA.CLI.Wallet.Parser (parseWalletCommand)

import EA.CLI.Run (RootCommand (..))

import Options.Applicative (
  Parser,
  command,
  hsubparser,
  info,
  progDesc,
 )

parseRootCommand :: Parser RootCommand
parseRootCommand =
  hsubparser
    ( command "auth" (info (AuthCommand <$> parseAuthCommand) (progDesc "Auth related command"))
        <> command "marketplace" (info (MarketplaceCommand <$> parseMarketplaceCommand) (progDesc "Marketplace related command"))
        <> command "oracle" (info (OracleCommand <$> parseOracleCommand) (progDesc "Oracle related command"))
        <> command "wallet" (info (WalletCommand <$> parseWalletCommand) (progDesc "Wallet related command"))
    )