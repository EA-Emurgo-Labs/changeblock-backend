module EA.Script (
  Scripts (..),
  carbonTokenMintingPolicy,
  carbonNftMintingPolicy,
  nftMintingPolicy,
  marketplaceValidator,
  oracleValidator,
) where

import EA.Internal (mintingPolicyFromPly, validatorFromPly)
import EA.Script.Marketplace (
  MarketplaceParams,
  MarketplaceScriptParams (..),
  marketPlaceParamsToScriptParams,
 )
import GeniusYield.Types
import PlutusLedgerApi.V1.Value (AssetClass)
import PlutusLedgerApi.V2 (
  CurrencySymbol,
  PubKeyHash,
  ScriptHash,
  TokenName,
  TxOutRef,
 )
import Ply (
  AsData (AsData),
  ScriptRole (MintingPolicyRole, ValidatorRole),
  TypedScript,
  (#),
 )

data Scripts = Scripts
  { scriptCarbonNftPolicy ::
      !(TypedScript 'MintingPolicyRole '[AsData TxOutRef, AsData TokenName])
  , scriptCarbonTokenPolicy ::
      !(TypedScript 'MintingPolicyRole '[AsData TokenName])
  , scriptMintingNftPolicy ::
      !(TypedScript 'MintingPolicyRole '[AsData TxOutRef])
  , scriptMarketplaceValidator ::
      !( TypedScript
          'ValidatorRole
          '[ AsData ScriptHash
           , AsData PubKeyHash
           , AsData TokenName
           , AsData CurrencySymbol
           , AsData TokenName
           , AsData PubKeyHash
           ]
       )
  , scriptOracleValidator ::
      !(TypedScript 'ValidatorRole '[AsData AssetClass, AsData PubKeyHash])
  }

carbonNftMintingPolicy ::
  GYTxOutRef -> GYTokenName -> Scripts -> GYMintingPolicy 'PlutusV2
carbonNftMintingPolicy oref tn scripts =
  mintingPolicyFromPly $
    scriptCarbonNftPolicy scripts
      # (AsData . txOutRefToPlutus $ oref)
      # (AsData . tokenNameToPlutus $ tn)

carbonTokenMintingPolicy :: GYTokenName -> Scripts -> GYMintingPolicy 'PlutusV2
carbonTokenMintingPolicy tn scripts =
  mintingPolicyFromPly $
    scriptCarbonTokenPolicy scripts
      # (AsData . tokenNameToPlutus $ tn)

nftMintingPolicy :: GYTxOutRef -> Scripts -> GYMintingPolicy 'PlutusV2
nftMintingPolicy oref scripts =
  mintingPolicyFromPly $
    scriptMintingNftPolicy scripts
      # (AsData . txOutRefToPlutus $ oref)

marketplaceValidator :: MarketplaceParams -> Scripts -> GYValidator 'PlutusV2
marketplaceValidator mktParam scripts =
  let MarketplaceScriptParams {..} = marketPlaceParamsToScriptParams mktParam
   in validatorFromPly $
        scriptMarketplaceValidator scripts
          # AsData mktSpOracleValidator
          # AsData mktSpEscrowValidator
          # AsData mktSpVersion
          # AsData mktSpOracleSymbol
          # AsData mktSpOracleTokenName
          # AsData mktSpBackdoor

oracleValidator ::
  GYAssetClass -> GYPaymentKeyHash -> Scripts -> GYValidator 'PlutusV2
oracleValidator asset pHash scripts =
  validatorFromPly $
    scriptOracleValidator scripts
      # (AsData . assetClassToPlutus $ asset)
      # (AsData . paymentKeyHashToPlutus $ pHash)
