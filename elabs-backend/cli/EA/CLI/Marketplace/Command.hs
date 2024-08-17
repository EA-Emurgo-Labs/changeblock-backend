module EA.CLI.Marketplace.Command (
  MarketplaceCommand (..),
  MarketplaceDeployCommand (..),
  MarketplaceWithdrawCommand (..),
) where

import Data.Text qualified as T
import GeniusYield.Types (GYMintingPolicyId, GYPaymentKeyHash)

data MarketplaceCommand
  = MarketplaceDeployScript MarketplaceDeployCommand
  | MarketplaceWithdrawScript MarketplaceWithdrawCommand

data MarketplaceDeployCommand = MarketplaceDeployCommand
  { mktDplRefScriptAddr :: !T.Text
  }

data MarketplaceWithdrawCommand = MarketplaceWithdrawCommand
  { mktWdrCarbonPolicyId :: !GYMintingPolicyId
  , mktWdrCarbonTokenName :: !T.Text
  , mktWdrOutAddress :: !T.Text
  , mktWdrQty :: !Natural
  , mktWdrBackdoorKeyPath :: !FilePath
  , mktWdrSellerPubKey :: !GYPaymentKeyHash
  }
