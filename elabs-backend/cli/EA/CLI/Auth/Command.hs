module EA.CLI.Auth.Command (
  AuthCommand (..),
  CreateAuthTokenCommand (..),
  CheckAuthTokenCommand (..),
) where

data AuthCommand
  = CreateAuthToken CreateAuthTokenCommand
  | CheckAuthToken CheckAuthTokenCommand

data CreateAuthTokenCommand = CreateAuthTokenCommand
  { crtAuthTokenVal :: !String
  , crtAuthTokenNotes :: !String
  }

data CheckAuthTokenCommand = CheckAuthTokenCommand
  { chkAuthTokenVal :: !String
  }