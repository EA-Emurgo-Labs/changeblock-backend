module EA.CLI.Oracle.Command (
  OracleCommand (..),
  OracleCreateCommand (..),
) where

import Data.Text qualified as T

data OracleCommand
  = CreateOracle OracleCreateCommand

data OracleCreateCommand = OracleCreateCommand
  { orcCreateRate :: !Natural
  , orcCreateAssetName :: !T.Text
  }