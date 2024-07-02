module EA.Api.Order.Exception (OrderApiException (..)) where

import EA.Script.Marketplace (MarketplaceInfo (..))
import EA.Script.Oracle (OracleInfo (..))
import GeniusYield.HTTP.Errors
import GeniusYield.Types (GYAddress, GYPaymentKeyHash)
import Network.HTTP.Types (status400)

data OrderApiException
  = OrderNoOracleUtxo
  | OrderNoOraclePolicyId {ordExpOracleInfo :: OracleInfo}
  | OrderNoOracleToken {ordExpOracleInfo :: OracleInfo}
  | OrderInvalidOwner {ordExpMarketplaceInfo :: MarketplaceInfo}
  | OrderNoOwnerAddress {ordExpMarketplaceInfo :: MarketplaceInfo}
  | OrderAlreadyOnSell {ordExpMarketplaceInfo :: MarketplaceInfo}
  | OrderAmountExceeds {ordExpMarketplaceInfo :: MarketplaceInfo, ordExpAmount :: Integer}
  | OrderInvalidBuyer {ordExpMarketplaceInfo :: MarketplaceInfo, ordExpBuyerAddr :: GYAddress}
  | NegativeOrderPrice {orderExpMarketplaceInfo :: MarketplaceInfo, ordExpPrice :: Integer}
  | SameOrderPrice {orderExpMarketplaceInfo :: MarketplaceInfo, ordExpPrice :: Integer}
  | OrderNotForSale {ordExpMarketplaceInfo :: MarketplaceInfo}
  | NonSellOrderPriceUpdate {ordExpMarketplaceInfo :: MarketplaceInfo}
  | InvalidBuyerPubKey {ordExpBuyerPubKey :: GYPaymentKeyHash}
  deriving stock (Show)
  deriving anyclass (Exception)

instance IsGYApiError OrderApiException where
  toApiError OrderNoOracleUtxo =
    GYApiError
      { gaeHttpStatus = status400
      , gaeErrorCode = "NO_ORACLE_UTXO_FOUND"
      , gaeMsg = "No UTxO found for oracle. Please contact Admin"
      }
  toApiError (OrderNoOraclePolicyId oracleInfo) =
    GYApiError
      { gaeHttpStatus = status400
      , gaeErrorCode = "NO_ORACLE_POLICY_ID"
      , gaeMsg = "No policy id found for oracle: " <> show (orcInfoUtxoRef oracleInfo) <> " \n Please contact Admin"
      }
  toApiError (OrderNoOracleToken oracleInfo) =
    GYApiError
      { gaeHttpStatus = status400
      , gaeErrorCode = "NO_ORACLE_TOKEN"
      , gaeMsg = "No token found for oracle: " <> show (orcInfoUtxoRef oracleInfo) <> " \n Please contact Admin"
      }
  toApiError (OrderInvalidOwner _marketplaceInfo) =
    GYApiError
      { gaeHttpStatus = status400
      , gaeErrorCode = "INVALID_OWNER"
      , gaeMsg = "Invalid owner for Marketplace. \n Only Owner can perform this action."
      }
  toApiError (OrderAlreadyOnSell _marketplaceInfo) =
    GYApiError
      { gaeHttpStatus = status400
      , gaeErrorCode = "ALREADY_ON_SELL"
      , gaeMsg = "Order is already on sell. Can only perform this action when order is not on sell."
      }
  toApiError (OrderAmountExceeds mInfo amount) =
    GYApiError
      { gaeHttpStatus = status400
      , gaeErrorCode = "AMOUNT_EXCEEDS"
      , gaeMsg = "Order amount exceeds the available amount. Available amount: " <> show (mktInfoTxOutRef mInfo) <> " Order amount: " <> show amount
      }
  toApiError (OrderInvalidBuyer _mInfo buyerAddr) =
    GYApiError
      { gaeHttpStatus = status400
      , gaeErrorCode = "INVALID_BUYER"
      , gaeMsg = "Invalid buyer for Marketplace. Buyer address: " <> show buyerAddr <> " is not valid."
      }
  toApiError (NegativeOrderPrice mInfo price) =
    GYApiError
      { gaeHttpStatus = status400
      , gaeErrorCode = "NEGATIVE_ORDER_PRICE"
      , gaeMsg = "Order price Must be greater than zero. Order price: " <> show price <> " for Marketplace: " <> show (mktInfoTxOutRef mInfo)
      }
  toApiError (SameOrderPrice mInfo price) =
    GYApiError
      { gaeHttpStatus = status400
      , gaeErrorCode = "SAME_ORDER_PRICE"
      , gaeMsg = "Order price Must be different than the current price. Order price: " <> show price <> " for Marketplace: " <> show (mktInfoTxOutRef mInfo)
      }
  toApiError (OrderNotForSale _mInfo) =
    GYApiError
      { gaeHttpStatus = status400
      , gaeErrorCode = "ORDER_NOT_FOR_SALE"
      , gaeMsg = "Order is not for sale."
      }
  toApiError (NonSellOrderPriceUpdate _mInfo) =
    GYApiError
      { gaeHttpStatus = status400
      , gaeErrorCode = "NON_SELL_ORDER_PRICE_UPDATE"
      , gaeMsg = "Cannot update price for non sell order."
      }
  toApiError (InvalidBuyerPubKey buyerPubKey) =
    GYApiError
      { gaeHttpStatus = status400
      , gaeErrorCode = "INVALID_BUYER"
      , gaeMsg = "No Buyer address found for pubkey: " <> show buyerPubKey
      }
  toApiError (OrderNoOwnerAddress mInfo) =
    GYApiError
      { gaeHttpStatus = status400
      , gaeErrorCode = "NO_OWNER_ADDRESS_FOUND"
      , gaeMsg = "No addresses found with Owner: " <> show (mktInfoOwner mInfo)
      }
