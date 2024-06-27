module EA.CLI.Run (
  runRootCommand,
  RootCommand (..),
) where

import EA.CLI.Auth.Command (AuthCommand)
import EA.CLI.Auth.Run (runAuthCommand)
import EA.CLI.Marketplace.Command (MarketplaceCommand)
import EA.CLI.Marketplace.Run (runMarketplaceCommand)
import EA.CLI.Oracle.Command (OracleCommand)
import EA.CLI.Oracle.Run (runOracleCommand)
import EA.CLI.Wallet.Command (WalletCommand)
import EA.CLI.Wallet.Run (runWalletCommand)

data RootCommand
  = AuthCommand !AuthCommand
  | MarketplaceCommand !MarketplaceCommand
  | OracleCommand !OracleCommand
  | WalletCommand !WalletCommand

runRootCommand :: RootCommand -> IO ()
runRootCommand (AuthCommand cmd) = runAuthCommand cmd
runRootCommand (MarketplaceCommand cmd) = runMarketplaceCommand cmd
runRootCommand (OracleCommand cmd) = runOracleCommand cmd
runRootCommand (WalletCommand cmd) = runWalletCommand cmd