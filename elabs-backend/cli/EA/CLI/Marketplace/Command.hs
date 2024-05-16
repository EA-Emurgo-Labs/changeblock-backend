module EA.CLI.Marketplace.Command (
  MarketplaceCommand (..),
  MarketplaceDeployCommand (..),
) where

import Data.Text qualified as T

data MarketplaceCommand
  = MarketplaceDeployScript MarketplaceDeployCommand

data MarketplaceDeployCommand = MarketplaceDeployCommand
  { mktDplRefScriptAddr :: !T.Text
  }
