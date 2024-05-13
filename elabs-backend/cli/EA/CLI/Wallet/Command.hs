module EA.CLI.Wallet.Command (
  WalletCommand (..),
  WalletInternalAddressCommand (..),
  WalletGenRootKeyCommand (..),
) where

data WalletCommand
  = WltHandleInternalAddress WalletInternalAddressCommand
  | WltGenRootKey WalletGenRootKeyCommand

data WalletInternalAddressCommand = WalletInternalAddressCommand
  { wltInternalAddressIsCollateral :: !Bool
  }

data WalletGenRootKeyCommand = WalletGenRootKeyCommand
  { wltGenRootKeyMnemonic :: !String
  }