module EA.CommonException (CommonException (..)) where

import Data.Text qualified as T
import EA.Api.Types (UserId (..))
import GeniusYield.HTTP.Errors (GYApiError (..), IsGYApiError (toApiError))
import GeniusYield.Types (GYAddress, GYPaymentKeyHash, GYTxOutRef)
import Network.HTTP.Types (status400)

data CommonException
  = EaNoUtxoForAddress {cmnExpEaAddressUtxo :: GYAddress}
  | EaNoInternalUtxo
  | EaInvalidTxRef {cmnExpEaTxRef :: GYTxOutRef}
  | EaNoCollateral
  | EaInvalidAddres {cmnExpEaAddress :: GYAddress}
  | EaCannotDecodeAddress {cmnExpEaPubKey :: GYPaymentKeyHash}
  | EaInvalidUserAddress {cmnExpEaUserId :: UserId}
  | EaCustomError {eaCustomErrorCode :: T.Text, eaCustomErrorMsg :: T.Text}
  deriving stock (Show)
  deriving anyclass (Exception)

instance IsGYApiError CommonException where
  toApiError (EaCustomError code msg) =
    GYApiError
      { gaeHttpStatus = status400
      , gaeErrorCode = code
      , gaeMsg = msg
      }
  toApiError EaNoInternalUtxo =
    GYApiError
      { gaeHttpStatus = status400
      , gaeErrorCode = "NO_UTXO_FOUND_INTERNAL_ADDRESS"
      , gaeMsg = "No UTxO found For Internal Address. Send some ADA to Internal Address"
      }
  toApiError (EaNoUtxoForAddress address) =
    GYApiError
      { gaeHttpStatus = status400
      , gaeErrorCode = "NO_UTXO_FOUND"
      , gaeMsg = "No UTxO Found for Address: " <> show address
      }
  toApiError EaNoCollateral =
    GYApiError
      { gaeHttpStatus = status400
      , gaeErrorCode = "NO_COLLATERAL_FOUND"
      , gaeMsg = "No collateral found "
      }
  toApiError (EaInvalidAddres address) =
    GYApiError
      { gaeHttpStatus = status400
      , gaeErrorCode = "INVALID_ADDRESS"
      , gaeMsg = "Invalid address: " <> show address
      }
  toApiError (EaCannotDecodeAddress pubKey) =
    GYApiError
      { gaeHttpStatus = status400
      , gaeErrorCode = "CANNOT_DECODE_ADDRESS"
      , gaeMsg = "Cannot decode address for pub key: " <> show pubKey
      }
  toApiError (EaInvalidTxRef txRef) =
    GYApiError
      { gaeHttpStatus = status400
      , gaeErrorCode = "INVALID_UTXO"
      , gaeMsg = "No UTxO Found for TxRef: " <> show txRef
      }
  toApiError (EaInvalidUserAddress (UserId uid)) =
    GYApiError
      { gaeHttpStatus = status400
      , gaeErrorCode = "INVALID_USER_ADDRESS"
      , gaeMsg = "No Address Found for User: " <> show uid
      }