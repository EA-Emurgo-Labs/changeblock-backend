module EA.CLI.Marketplace.Command (
  MarketplaceCommand (..),
  MarketplaceDeployCommand (..),
) where

data MarketplaceCommand
  = MarketplaceDeployScript MarketplaceDeployCommand

data MarketplaceDeployCommand = MarketplaceDeployCommand
  { mktDplRefScriptAddr :: !String
  }
