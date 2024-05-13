module EA.CLI.Auth.Parser (
  parseAuthCommand,
) where

import EA.CLI.Auth.Command (
  AuthCommand (..),
  CheckAuthTokenCommand (CheckAuthTokenCommand),
  CreateAuthTokenCommand (CreateAuthTokenCommand),
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

parseAuthCommand :: Parser AuthCommand
parseAuthCommand =
  hsubparser
    ( command
        "create"
        ( info
            (CreateAuthToken <$> pCreateAuthTokenCommand)
            (progDesc "Create a new auth token")
        )
        <> command
          "check"
          ( info
              (CheckAuthToken <$> pCheckAuthTokenCommand)
              (progDesc "Check an auth token")
          )
    )

pCreateAuthTokenCommand :: Parser CreateAuthTokenCommand
pCreateAuthTokenCommand =
  CreateAuthTokenCommand
    <$> strOption
      ( long "value"
          <> metavar "STRING"
          <> help "Value of the token"
      )
    <*> strOption
      ( long "notes"
          <> metavar "STRING"
          <> help "Notes for the token"
      )

pCheckAuthTokenCommand :: Parser CheckAuthTokenCommand
pCheckAuthTokenCommand =
  CheckAuthTokenCommand
    <$> strOption
      ( long "value"
          <> metavar "STRING"
          <> help "Value of the token To check"
      )