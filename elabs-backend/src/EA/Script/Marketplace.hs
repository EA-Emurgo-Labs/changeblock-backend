{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module EA.Script.Marketplace (
  MarketplaceAction (..),
  MarketplaceScriptParams (..),
  MarketplaceDatum (..),
  MarketplaceParams (..),
  MarketplaceInfo (..),
  MarketplaceOrderType (..),
  marketPlaceParamsToScriptParams,
  marketplaceInfoToDatum,
  marketplaceDatumToInfo,
) where

import Control.Lens ((.~), (?~))
import Data.Aeson (FromJSON (parseJSON))
import Data.Aeson qualified as Aeson
import Data.Swagger qualified as Swagger
import Data.Swagger.Internal.Schema qualified as Swagger
import Data.Text qualified as T
import GeniusYield.Types
import PlutusLedgerApi.V1.Value (assetClass)
import PlutusLedgerApi.V2 (CurrencySymbol, PubKeyHash, ScriptHash, TokenName)
import PlutusTx qualified
import PlutusTx.Prelude qualified as PlutusTx
import Servant (FromHttpApiData (parseUrlPiece), ToHttpApiData (toUrlPiece))

data MarketplaceAction
  = BUY
  | MERGE
  | SELL {newPrice :: Integer}
  | CANCEL
  | BUY_PARTIAL {amt :: Integer}
  deriving stock (Show)

PlutusTx.makeIsDataIndexed ''MarketplaceAction [('BUY, 0), ('MERGE, 1), ('SELL, 2), ('CANCEL, 3), ('BUY_PARTIAL, 4)]

data MarketplaceScriptParams = MarketplaceScriptParams
  { mktSpOracleValidator :: ScriptHash
  -- ^ The script Hash of Oracle Validator
  , mktSpEscrowValidator :: PubKeyHash
  -- ^ The PubKeyHash of Escrow. Fee will be sent to this address
  , mktSpVersion :: TokenName
  -- ^ This is just used to make different version of Marketplace while testing.
  , mktSpOracleSymbol :: CurrencySymbol
  -- ^ The policyId of Oracle Token
  , mktSpOracleTokenName :: TokenName
  -- ^ The TokenName of Oracle Token
  , mktSpBackdoor :: PubKeyHash
  -- ^ The PubKeyHash of Backdoor
  }
  deriving stock (Show)

PlutusTx.unstableMakeIsData ''MarketplaceScriptParams

data MarketplaceDatum = MarketplaceDatum
  { mktDtmOwner :: PubKeyHash
  -- ^ The owner of the Order
  , mktDtmSalePrice :: PlutusTx.Integer
  -- ^ The Sale Price
  , mktDtmAssetSymbol :: CurrencySymbol
  -- ^ The policyId of Carbon Token
  , mktDtmAssetName :: TokenName
  -- ^ The TokenName of Carbon Token
  , mktDtmAmount :: PlutusTx.Integer
  -- ^ The Amount of Carbon Token
  , mktDtmIssuer :: PubKeyHash
  -- ^ The issuer of the Order. One who minted the token
  , mktDtmIsSell :: PlutusTx.Integer
  -- ^ The type of Order. 1 if is OnSell else 0
  }
  deriving stock (Show)

PlutusTx.unstableMakeIsData ''MarketplaceDatum

data MarketplaceParams = MarketplaceParams
  { mktPrmOracleValidator :: GYValidatorHash
  , mktPrmEscrowValidator :: GYPaymentKeyHash
  , mktPrmVersion :: GYTokenName
  , mktPrmOracleSymbol :: GYMintingPolicyId
  , mktPrmOracleTokenName :: GYTokenName
  , mktPrmBackdoor :: GYPaymentKeyHash
  }
  deriving stock (Show)

marketPlaceParamsToScriptParams :: MarketplaceParams -> MarketplaceScriptParams
marketPlaceParamsToScriptParams MarketplaceParams {..} =
  MarketplaceScriptParams
    { mktSpOracleValidator = validatorHashToPlutus mktPrmOracleValidator
    , mktSpEscrowValidator = paymentKeyHashToPlutus mktPrmEscrowValidator
    , mktSpVersion = tokenNameToPlutus mktPrmVersion
    , mktSpOracleSymbol = mintingPolicyIdToCurrencySymbol mktPrmOracleSymbol
    , mktSpOracleTokenName = tokenNameToPlutus mktPrmOracleTokenName
    , mktSpBackdoor = paymentKeyHashToPlutus mktPrmBackdoor
    }

data MarketplaceOrderType = M_BUY | M_SELL
  deriving stock (Enum, Show, Eq, Generic)

instance Swagger.ToSchema MarketplaceOrderType where
  declareNamedSchema _ = do
    return $
      Swagger.named "MarketplaceOrderType" $
        mempty
          & Swagger.type_
          ?~ Swagger.SwaggerString
            & Swagger.enum_
          ?~ ["BUY", "SELL"]
            & Swagger.description
          ?~ "Marketplace Order Type"
            & Swagger.maxItems
          ?~ 1
            & Swagger.minItems
          ?~ 1

instance Swagger.ToParamSchema MarketplaceOrderType where
  toParamSchema _ =
    mempty
      & Swagger.type_
      ?~ Swagger.SwaggerString
        & Swagger.enum_
      ?~ ["BUY", "SELL"]
        & Swagger.maxItems
      ?~ 1
        & Swagger.minItems
      ?~ 1

instance Aeson.FromJSON MarketplaceOrderType where
  parseJSON = Aeson.withText "MarketplaceOrderType" $ \case
    "BUY" -> pure M_BUY
    "SELL" -> pure M_SELL
    _ -> fail "Invalid MarketplaceOrderType: "

instance Aeson.ToJSON MarketplaceOrderType where
  toJSON M_BUY = Aeson.String "BUY"
  toJSON M_SELL = Aeson.String "SELL"

instance FromHttpApiData MarketplaceOrderType where
  parseUrlPiece t = either (Left . T.pack) Right $ Aeson.eitherDecode $ Aeson.encode t

instance ToHttpApiData MarketplaceOrderType where
  toUrlPiece M_BUY = "BUY"
  toUrlPiece M_SELL = "SELL"

data MarketplaceInfo = MarketplaceInfo
  { mktInfoTxOutRef :: GYTxOutRef
  , mktInfoAddress :: GYAddress
  , mktInfoValue :: GYValue
  , mktInfoOwner :: GYPaymentKeyHash
  , mktInfoSalePrice :: Integer
  , mktInfoCarbonPolicyId :: GYMintingPolicyId
  , mktInfoCarbonAssetName :: GYTokenName
  , mktInfoAmount :: Integer
  , mktInfoIssuer :: GYPaymentKeyHash
  , mktInfoIsSell :: MarketplaceOrderType
  , mktInfoAdaPrice :: Maybe Double
  , mktInfoTokenPrice :: Maybe Double
  }
  deriving stock (Show, Generic)

instance Aeson.ToJSON MarketplaceInfo where
  toJSON MarketplaceInfo {..} =
    Aeson.object
      [ "tx_ref" Aeson..= mktInfoTxOutRef
      , "address" Aeson..= mktInfoAddress
      , "value" Aeson..= mktInfoValue
      , "owner" Aeson..= mktInfoOwner
      , "price" Aeson..= mktInfoSalePrice
      , "carbon_token_id" Aeson..= mktInfoCarbonPolicyId
      , "carbon_token_name" Aeson..= mktInfoCarbonAssetName
      , "amount" Aeson..= mktInfoAmount
      , "issuer" Aeson..= mktInfoIssuer
      , "order_type" Aeson..= mktInfoIsSell
      , "ada_price_usd" Aeson..= mktInfoAdaPrice
      , "token_price_in_usd" Aeson..= mktInfoTokenPrice
      ]

instance Aeson.FromJSON MarketplaceInfo where
  parseJSON = Aeson.withObject "MarketplaceInfo" $ \v ->
    MarketplaceInfo
      <$> v
        Aeson..: "tx_ref"
      <*> v
        Aeson..: "address"
      <*> v
        Aeson..: "value"
      <*> v
        Aeson..: "owner"
      <*> v
        Aeson..: "price"
      <*> v
        Aeson..: "carbon-token-id"
      <*> v
        Aeson..: "carbon-token-name"
      <*> v
        Aeson..: "amount"
      <*> v
        Aeson..: "issuer"
      <*> v
        Aeson..: "order_type"
      <*> v
        Aeson..: "ada_price_usd"
      <*> v
        Aeson..: "token_price_in_usd"

instance Swagger.ToSchema MarketplaceInfo where
  declareNamedSchema _ = do
    txOutRefSchema <- Swagger.declareSchemaRef @GYTxOutRef Proxy
    addressSchema <- Swagger.declareSchemaRef @GYAddress Proxy
    valueSchema <- Swagger.declareSchemaRef @GYValue Proxy
    pubKeyHashSchema <- Swagger.declareSchemaRef @GYPaymentKeyHash Proxy
    mintingPolicyIdSchema <- Swagger.declareSchemaRef @GYMintingPolicyId Proxy
    tokenNameSchema <- Swagger.declareSchemaRef @GYTokenName Proxy
    orderTypeSchema <- Swagger.declareSchemaRef @MarketplaceOrderType Proxy
    integerSchema <- Swagger.declareSchemaRef @Integer Proxy
    doubleSchema <- Swagger.declareSchemaRef @Double Proxy
    return $
      Swagger.named "MarketplaceInfo" $
        mempty
          & Swagger.type_
          ?~ Swagger.SwaggerObject
            & Swagger.properties
          .~ [ (T.pack "tx_ref", txOutRefSchema)
             , (T.pack "address", addressSchema)
             , (T.pack "value", valueSchema)
             , (T.pack "owner", pubKeyHashSchema)
             , (T.pack "price", integerSchema)
             , (T.pack "carbon-token-id", mintingPolicyIdSchema)
             , (T.pack "carbon-token-name", tokenNameSchema)
             , (T.pack "amount", integerSchema)
             , (T.pack "issuer", pubKeyHashSchema)
             , (T.pack "order_type", orderTypeSchema)
             , (T.pack "ada_price_usd", doubleSchema)
             , (T.pack "token_price_in_usd", doubleSchema)
             ]
            & Swagger.required
          .~ [T.pack "tx_ref", T.pack "address", T.pack "value", T.pack "owner", T.pack "price", T.pack "carbon-token-id", T.pack "carbon-token-name", T.pack "amount", T.pack "issuer", T.pack "order_type"]
            & Swagger.description
          ?~ "Marketplace Order Info"
            & Swagger.maxProperties
          ?~ 12
            & Swagger.minProperties
          ?~ 12

marketplaceInfoToDatum :: MarketplaceInfo -> MarketplaceDatum
marketplaceInfoToDatum MarketplaceInfo {..} =
  MarketplaceDatum
    { mktDtmOwner = paymentKeyHashToPlutus mktInfoOwner
    , mktDtmSalePrice = mktInfoSalePrice
    , mktDtmAssetSymbol = mintingPolicyIdToCurrencySymbol mktInfoCarbonPolicyId
    , mktDtmAssetName = tokenNameToPlutus mktInfoCarbonAssetName
    , mktDtmAmount = mktInfoAmount
    , mktDtmIssuer = paymentKeyHashToPlutus mktInfoIssuer
    , mktDtmIsSell = toInteger $ fromEnum mktInfoIsSell
    }

marketplaceDatumToInfo ::
  GYTxOutRef ->
  GYValue ->
  GYAddress ->
  MarketplaceDatum ->
  Maybe Double ->
  Either String MarketplaceInfo
marketplaceDatumToInfo oref val addr datum mAdaPrice = do
  pubkeyIssuer <- seither . pubKeyHashFromPlutus $ mktDtmIssuer datum
  pubkeyOwner <- seither . pubKeyHashFromPlutus $ mktDtmOwner datum
  (tokenPolicy, tokenName) <-
    seither $
      unpackAc
        <$> assetClassFromPlutus
          (assetClass (mktDtmAssetSymbol datum) (mktDtmAssetName datum))
  let salePrice = mktDtmSalePrice datum
      tknPrice = (`tokenPrice` salePrice) =<< mAdaPrice
  return
    MarketplaceInfo
      { mktInfoTxOutRef = oref
      , mktInfoAddress = addr
      , mktInfoValue = val
      , mktInfoOwner = paymentKeyHashFromApi $ pubKeyHashToApi pubkeyOwner
      , mktInfoSalePrice = salePrice
      , mktInfoCarbonPolicyId = tokenPolicy
      , mktInfoCarbonAssetName = tokenName
      , mktInfoAmount = mktDtmAmount datum
      , mktInfoIssuer = paymentKeyHashFromApi $ pubKeyHashToApi pubkeyIssuer
      , mktInfoIsSell = toEnum $ fromInteger $ mktDtmIsSell datum
      , mktInfoAdaPrice = mAdaPrice
      , mktInfoTokenPrice = tknPrice
      }
  where
    seither :: (Show b) => Either b a -> Either String a
    seither = either (Left . show) Right

    unpackAc :: GYAssetClass -> (GYMintingPolicyId, GYTokenName)
    unpackAc (GYToken tokenPolicy tokenName) = (tokenPolicy, tokenName)
    unpackAc _ = ("", "")

    tokenPrice :: Double -> Integer -> Maybe Double
    tokenPrice adaUsdPrice tokenLovelacePriceRate = Just $ (fromIntegral tokenLovelacePriceRate / 1000000) * adaUsdPrice
