module EA.CLI.Oracle.Parser (
  parseOracleCommand,
) where

import EA.CLI.Oracle.Command (
  OracleCommand (..),
  OracleCreateCommand (OracleCreateCommand),
 )

import Options.Applicative (
  Parser,
  auto,
  command,
  help,
  helper,
  info,
  long,
  metavar,
  option,
  progDesc,
  showDefault,
  strOption,
  subparser,
  value,
 )

parseOracleCommand :: Parser OracleCommand
parseOracleCommand =
  subparser $
    mconcat
      [ metavar "oracle Commands"
      , command
          "create"
          ( info (CreateOracle <$> pOracleCreateCommand <**> helper) $
              progDesc "Create Oracle"
          )
      ]

pOracleCreateCommand :: Parser OracleCreateCommand
pOracleCreateCommand =
  OracleCreateCommand
    <$> option
      auto
      ( long "rate"
          <> metavar "RATE"
          <> help "Rate for oracle in lovelace"
      )
    <*> strOption
      ( long "asset"
          <> metavar "ASSET_NAME"
          <> help "Asset Name for oracle"
          <> showDefault
          <> value "43424c"
      )