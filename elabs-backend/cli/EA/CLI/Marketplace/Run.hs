module EA.CLI.Marketplace.Run (runMarketplaceCommand) where

import EA.CLI.Marketplace.Command (MarketplaceCommand)

runMarketplaceCommand :: MarketplaceCommand -> IO ()
runMarketplaceCommand _ = do
  putStrLn "Hello runMarketplaceCommand"