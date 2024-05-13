module EA.CLI.Marketplace.Parser (
  parseMarketplaceCommand,
) where

import EA.CLI.Marketplace.Command (
  MarketplaceCommand (..),
  MarketplaceDeployCommand (MarketplaceDeployCommand),
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
  strOption,
 )

parseMarketplaceCommand :: Parser MarketplaceCommand
parseMarketplaceCommand =
  hsubparser
    ( command
        "deploy"
        ( info (MarketplaceDeployScript <$> pMarketplaceDeployCommand) $
            progDesc "Deploy Marketplace Reference Script"
        )
    )

pMarketplaceDeployCommand :: Parser MarketplaceDeployCommand
pMarketplaceDeployCommand =
  MarketplaceDeployCommand
    <$> strOption
      ( long "address"
          <> metavar "STRING"
          <> help "Address to send Marketplace Reference Script utxo"
      )