module EA.CLI.Marketplace.Parser (
  parseMarketplaceCommand,
) where

import EA.CLI.Marketplace.Command (
  MarketplaceCommand (..),
  MarketplaceDeployCommand (MarketplaceDeployCommand),
  MarketplaceWithdrawCommand (MarketplaceWithdrawCommand),
 )
import Options.Applicative (
  Parser,
  auto,
  command,
  help,
  hsubparser,
  info,
  long,
  metavar,
  option,
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
        <> command
          "withdraw"
          ( info (MarketplaceWithdrawScript <$> pMarketplaceWithdrawCommand) $
              progDesc "Withdraw Carbon Tokens"
          )
    )

pMarketplaceDeployCommand :: Parser MarketplaceDeployCommand
pMarketplaceDeployCommand =
  MarketplaceDeployCommand
    <$> strOption
      ( long "address"
          <> metavar "ADDRESS"
          <> help "Address to send Marketplace Reference Script utxo"
      )

pMarketplaceWithdrawCommand :: Parser MarketplaceWithdrawCommand
pMarketplaceWithdrawCommand =
  MarketplaceWithdrawCommand
    <$> strOption
      ( long "carbon-policy-id"
          <> metavar "CARBON_POLICY_ID"
          <> help "Carbon Policy Id"
      )
    <*> strOption
      ( long "carbon-token-name"
          <> metavar "CARBON_TOKEN_NAME"
          <> help "Carbon Token Name"
      )
    <*> strOption
      ( long "out-address"
          <> metavar "OUT_ADDRESS"
          <> help "Address to send Carbon Tokens"
      )
    <*> option
      auto
      ( long "qty"
          <> metavar "QTY"
          <> help "Quantity of Carbon Tokens to send"
      )
    <*> strOption
      ( long "backdoor-key-path"
          <> metavar "BACKDOOR_KEY_PATH"
          <> help "Path to backdoor key"
      )
    <*> strOption
      ( long "token-owner"
          <> metavar "CARBON_TOKEN_OWNER"
          <> help "Carbon token owner"
      )