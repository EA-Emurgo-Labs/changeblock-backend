module EA.CLI.Oracle.Command (
  OracleCommand (..),
  OracleCreateCommand (..),
) where

data OracleCommand
  = CreateOracle OracleCreateCommand

data OracleCreateCommand = OracleCreateCommand
  { orcCreateRate :: !Natural
  , orcCreateAssetName :: !String
  }