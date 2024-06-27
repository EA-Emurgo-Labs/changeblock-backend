module EA.CLI.Wallet.Run (
  runWalletCommand,
) where

import Cardano.Mnemonic (MkSomeMnemonic (mkSomeMnemonic))
import Data.Text qualified as T
import EA.CLI.Helper (fetchCoreCfg, fetchRootKey, fetchSqlPool)
import EA.CLI.Wallet.Command (WalletCommand (..), WalletGenRootKeyCommand (..), WalletInternalAddressCommand (..))
import EA.Wallet (eaGetInternalAddressesIO)
import GeniusYield.GYConfig (GYCoreConfig (cfgNetworkId))
import Internal.Wallet (genRootKeyFromMnemonic, writeRootKey)

runWalletCommand :: WalletCommand -> IO ()
-- Fetch internal address
runWalletCommand (WltHandleInternalAddress cmd) = do
  pool <- fetchSqlPool
  coreCfg <- fetchCoreCfg
  rootKey <- fetchRootKey
  addrs <- eaGetInternalAddressesIO (wltInternalAddressIsCollateral cmd) (cfgNetworkId coreCfg) rootKey pool
  putTextLn . show $ fst <$> addrs

-- Generate Root Key Command
runWalletCommand (WltGenRootKey cmd) = do
  mw <-
    either
      (const (error "Invalid mnemonic"))
      return
      (mkSomeMnemonic @'[24] (words $ T.pack $ wltGenRootKeyMnemonic cmd))
  writeRootKey (wltRootKeyPath cmd) $ genRootKeyFromMnemonic mw