{-# LANGUAGE OverloadedLists #-}

module EA.Api.Order.Types (
  OrderSellRequest (..),
  OrderBuyRequest (..),
  OrderCancelRequest (..),
  OrderUpdateRequest (..),
) where

import Control.Lens ((.~), (?~))
import Data.Aeson qualified as Aeson
import Data.Swagger qualified as Swagger
import Data.Swagger.Internal.Schema qualified as Swagger
import GeniusYield.Types

import Data.Text qualified as T

data OrderSellRequest = OrderSellRequest
  { owner :: !GYPaymentKeyHash
  -- ^ The pubkeyHash of The owner of the order.
  , sellReqAmount :: !Natural
  -- ^ The amount of carbon to mint.
  , sellReqPrice :: !Natural
  -- ^ The sell price per unit of carbon.
  , sellReqOrderUtxoRef :: !GYTxOutRef
  -- ^ The order UTXO reference.
  }
  deriving stock (Show, Generic)

ownerTag, amountTag, priceTag, utxoTag, newPriceTag, buyerTag :: String
ownerTag = "owner"
amountTag = "amount"
priceTag = "price"
utxoTag = "tx_ref"
newPriceTag = "new_price"
buyerTag = "buyer"

instance Aeson.FromJSON OrderSellRequest where
  parseJSON = Aeson.withObject "OrderSellRequest" $ \v ->
    OrderSellRequest
      <$> v
      Aeson..: fromString ownerTag
      <*> v
      Aeson..: fromString amountTag
      <*> v
      Aeson..: fromString priceTag
      <*> v
      Aeson..: fromString utxoTag

instance Aeson.ToJSON OrderSellRequest where
  toEncoding OrderSellRequest {..} =
    Aeson.pairs $
      fromString ownerTag
        Aeson..= owner
        <> fromString amountTag
        Aeson..= sellReqAmount
        <> fromString priceTag
        Aeson..= sellReqPrice
        <> fromString utxoTag
        Aeson..= sellReqOrderUtxoRef

instance Swagger.ToSchema OrderSellRequest where
  declareNamedSchema _ = do
    gYPaymentKeyHashSchema <- Swagger.declareSchemaRef @GYPaymentKeyHash Proxy
    naturalSchema <- Swagger.declareSchemaRef @Natural Proxy
    gytxOutRefSchema <- Swagger.declareSchemaRef @GYTxOutRef Proxy
    return $
      Swagger.named "OrderSellRequestParam" $
        mempty
          & Swagger.type_ ?~ Swagger.SwaggerObject
          & Swagger.properties
            .~ [ (T.pack ownerTag, gYPaymentKeyHashSchema)
               , (T.pack amountTag, naturalSchema)
               , (T.pack priceTag, naturalSchema)
               , (T.pack utxoTag, gytxOutRefSchema)
               ]
          & Swagger.required .~ [T.pack ownerTag, T.pack amountTag, T.pack priceTag, T.pack utxoTag]
          & Swagger.description ?~ "Param to put order in sale"
          & Swagger.maxProperties ?~ 4
          & Swagger.minProperties ?~ 4

data OrderUpdateRequest = OrderUpdateRequest
  { owner :: !GYPaymentKeyHash
  -- ^ The user ID. The owner of the order.
  , updatedPrice :: !Natural
  -- ^ The sell price per unit of carbon.
  , orderUtxoRef :: !GYTxOutRef
  -- ^ The order UTXO reference.
  }
  deriving stock (Show, Generic)

instance Aeson.FromJSON OrderUpdateRequest where
  parseJSON = Aeson.withObject "OrderUpdateRequest" $ \v ->
    OrderUpdateRequest
      <$> v
      Aeson..: fromString ownerTag
      <*> v
      Aeson..: fromString newPriceTag
      <*> v
      Aeson..: fromString utxoTag

instance Aeson.ToJSON OrderUpdateRequest where
  toEncoding OrderUpdateRequest {..} =
    Aeson.pairs $
      fromString ownerTag
        Aeson..= owner
        <> fromString newPriceTag
        Aeson..= updatedPrice
        <> fromString utxoTag
        Aeson..= orderUtxoRef

instance Swagger.ToSchema OrderUpdateRequest where
  declareNamedSchema _ = do
    gYPaymentKeyHashSchema <- Swagger.declareSchemaRef @GYPaymentKeyHash Proxy
    naturalSchema <- Swagger.declareSchemaRef @Natural Proxy
    gytxOutRefSchema <- Swagger.declareSchemaRef @GYTxOutRef Proxy
    return $
      Swagger.named "OrderUpdateRequestParam" $
        mempty
          & Swagger.type_ ?~ Swagger.SwaggerObject
          & Swagger.properties
            .~ [ (T.pack ownerTag, gYPaymentKeyHashSchema)
               , (T.pack newPriceTag, naturalSchema)
               , (T.pack utxoTag, gytxOutRefSchema)
               ]
          & Swagger.required .~ [T.pack ownerTag, T.pack newPriceTag, T.pack utxoTag]
          & Swagger.description ?~ "Param to update price order in sale"
          & Swagger.maxProperties ?~ 3
          & Swagger.minProperties ?~ 3

data OrderCancelRequest = OrderCancelRequest
  { owner :: !GYPaymentKeyHash
  -- ^ The user ID who is owner of the order.
  , cancelOrderUtxo :: !GYTxOutRef
  -- ^ The order UTXO reference.
  }
  deriving stock (Show, Generic)

instance Aeson.FromJSON OrderCancelRequest where
  parseJSON = Aeson.withObject "OrderCancelRequest" $ \v ->
    OrderCancelRequest
      <$> v
      Aeson..: fromString ownerTag
      <*> v
      Aeson..: fromString utxoTag

instance Swagger.ToSchema OrderCancelRequest where
  declareNamedSchema _ = do
    gYPaymentKeyHashSchema <- Swagger.declareSchemaRef @GYPaymentKeyHash Proxy
    gytxOutRefSchema <- Swagger.declareSchemaRef @GYTxOutRef Proxy
    return $
      Swagger.named "OrderCancelRequestParam" $
        mempty
          & Swagger.type_ ?~ Swagger.SwaggerObject
          & Swagger.properties
            .~ [ (T.pack ownerTag, gYPaymentKeyHashSchema)
               , (T.pack utxoTag, gytxOutRefSchema)
               ]
          & Swagger.required .~ [T.pack ownerTag, T.pack utxoTag]
          & Swagger.description ?~ "Param to cancel order in sale"
          & Swagger.maxProperties ?~ 2
          & Swagger.minProperties ?~ 2

data OrderBuyRequest = OrderBuyRequest
  { buyer :: !GYPaymentKeyHash
  -- ^ The user ID.
  , buyAmount :: !Natural
  -- ^ The amount of carbon to buy.
  , orderUtxo :: !GYTxOutRef
  }
  deriving stock (Show, Generic)

instance Aeson.FromJSON OrderBuyRequest where
  parseJSON = Aeson.withObject "OrderBuyRequest" $ \v ->
    OrderBuyRequest
      <$> v
      Aeson..: fromString buyerTag
      <*> v
      Aeson..: fromString amountTag
      <*> v
      Aeson..: fromString utxoTag

instance Aeson.ToJSON OrderBuyRequest where
  toEncoding OrderBuyRequest {..} =
    Aeson.pairs $
      fromString buyerTag
        Aeson..= buyer
        <> fromString amountTag
        Aeson..= buyAmount
        <> fromString utxoTag
        Aeson..= orderUtxo
instance Swagger.ToSchema OrderBuyRequest where
  declareNamedSchema _ = do
    gYPaymentKeyHashSchema <- Swagger.declareSchemaRef @GYPaymentKeyHash Proxy
    naturalSchema <- Swagger.declareSchemaRef @Natural Proxy
    gytxOutRefSchema <- Swagger.declareSchemaRef @GYTxOutRef Proxy
    return $
      Swagger.named "OrderBuyRequestParam" $
        mempty
          & Swagger.type_ ?~ Swagger.SwaggerObject
          & Swagger.properties
            .~ [ (T.pack buyerTag, gYPaymentKeyHashSchema)
               , (T.pack amountTag, naturalSchema)
               , (T.pack utxoTag, gytxOutRefSchema)
               ]
          & Swagger.required .~ [T.pack buyerTag, T.pack amountTag, T.pack utxoTag]
          & Swagger.description ?~ "Param to buy order in sale"
          & Swagger.maxProperties ?~ 3
          & Swagger.minProperties ?~ 3