module EA.CLI.Wallet.Run (
  runWalletCommand,
) where

import EA.CLI.Wallet.Command (WalletCommand)

runWalletCommand :: WalletCommand -> IO ()
runWalletCommand _ = do
  putStrLn "Hello runWalletCommand"
  return ()