{-# LANGUAGE TemplateHaskell #-}

module EA.Script.Marketplace (
  MarketplaceAction (..),
  MarketplaceScriptParams (..),
  MarketplaceDatum (..),
  MarketplaceParams (..),
  MarketplaceInfo (..),
  marketPlaceParamsToScriptParams,
  marketplaceInfoToDatum,
  marketplaceDatumToInfo,
) where

import Data.Aeson qualified as Aeson
import Data.Swagger qualified as Swagger
import GeniusYield.Types
import PlutusLedgerApi.V1 (CurrencySymbol, PubKeyHash, ScriptHash, TokenName)
import PlutusLedgerApi.V1.Value (assetClass)
import PlutusTx qualified
import PlutusTx.Prelude qualified as PlutusTx

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
  , mktPrmEscrowValidator :: GYPubKeyHash
  , mktPrmVersion :: GYTokenName
  , mktPrmOracleSymbol :: GYMintingPolicyId
  , mktPrmOracleTokenName :: GYTokenName
  }
  deriving stock (Show)

marketPlaceParamsToScriptParams :: MarketplaceParams -> MarketplaceScriptParams
marketPlaceParamsToScriptParams MarketplaceParams {..} =
  MarketplaceScriptParams
    { mktSpOracleValidator = validatorHashToPlutus mktPrmOracleValidator
    , mktSpEscrowValidator = pubKeyHashToPlutus mktPrmEscrowValidator
    , mktSpVersion = tokenNameToPlutus mktPrmVersion
    , mktSpOracleSymbol = mintingPolicyIdCurrencySymbol mktPrmOracleSymbol
    , mktSpOracleTokenName = tokenNameToPlutus mktPrmOracleTokenName
    }

data MarketplaceInfo = MarketplaceInfo
  { mktInfoTxOutRef :: GYTxOutRef
  , mktInfoAddress :: GYAddress
  , mktInfoValue :: GYValue
  , mktInfoOwner :: GYPubKeyHash
  , mktInfoSalePrice :: Integer
  , mktInfoCarbonPolicyId :: GYMintingPolicyId
  , mktInfoCarbonAssetName :: GYTokenName
  , mktInfoAmount :: Integer
  , mktInfoIssuer :: GYPubKeyHash
  , mktInfoIsSell :: Integer
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.ToJSON, Swagger.ToSchema)

marketplaceInfoToDatum :: MarketplaceInfo -> MarketplaceDatum
marketplaceInfoToDatum MarketplaceInfo {..} =
  MarketplaceDatum
    { mktDtmOwner = pubKeyHashToPlutus mktInfoOwner
    , mktDtmSalePrice = mktInfoSalePrice
    , mktDtmAssetSymbol = mintingPolicyIdCurrencySymbol mktInfoCarbonPolicyId
    , mktDtmAssetName = tokenNameToPlutus mktInfoCarbonAssetName
    , mktDtmAmount = mktInfoAmount
    , mktDtmIssuer = pubKeyHashToPlutus mktInfoIssuer
    , mktDtmIsSell = mktInfoIsSell
    }

marketplaceDatumToInfo ::
  GYTxOutRef ->
  GYValue ->
  GYAddress ->
  MarketplaceDatum ->
  Either String MarketplaceInfo
marketplaceDatumToInfo oref val addr datum = do
  pubkeyIssuer <- seither . pubKeyHashFromPlutus $ mktDtmIssuer datum
  pubkeyOwner <- seither . pubKeyHashFromPlutus $ mktDtmOwner datum
  (tokenPolicy, tokenName) <-
    seither $
      unpackAc
        <$> assetClassFromPlutus
          (assetClass (mktDtmAssetSymbol datum) (mktDtmAssetName datum))

  return
    MarketplaceInfo
      { mktInfoTxOutRef = oref
      , mktInfoAddress = addr
      , mktInfoValue = val
      , mktInfoOwner = pubkeyOwner
      , mktInfoSalePrice = mktDtmSalePrice datum
      , mktInfoCarbonPolicyId = tokenPolicy
      , mktInfoCarbonAssetName = tokenName
      , mktInfoAmount = mktDtmAmount datum
      , mktInfoIssuer = pubkeyIssuer
      , mktInfoIsSell = mktDtmIsSell datum
      }
  where
    seither :: (Show b) => Either b a -> Either String a
    seither = either (Left . show) Right

    unpackAc :: GYAssetClass -> (GYMintingPolicyId, GYTokenName)
    unpackAc (GYToken tokenPolicy tokenName) = (tokenPolicy, tokenName)
    unpackAc _ = ("", "")
