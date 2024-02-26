module EA.Tx.Changeblock.Marketplace (buy, partialBuy, sell, cancel, merge, merge') where

import EA ()
import EA.Script (Scripts, marketplaceValidator)
import EA.Script.Marketplace (MarketplaceAction, MarketplaceDatum (..), MarketplaceInfo (..), MarketplaceParams (..), marketplaceInfoToDatum)
import EA.Script.Marketplace qualified as Marketplace
import EA.Script.Oracle (OracleInfo (..))
import GeniusYield.TxBuilder (
  GYTxSkeleton,
  mustBeSignedBy,
  mustHaveInput,
  mustHaveOutput,
  mustHaveRefInput,
 )
import GeniusYield.Types (
  GYAssetClass (GYToken),
  GYInScript (GYInReference, GYInScript),
  GYNetworkId,
  GYPubKeyHash,
  GYTxIn (GYTxIn, gyTxInTxOutRef, gyTxInWitness),
  GYTxInWitness (GYTxInWitnessScript),
  GYTxOutRef,
  GYValidator,
  GYValue,
  PlutusVersion (PlutusV2),
  addressFromPubKeyHash,
  datumFromPlutusData,
  mintingPolicyIdCurrencySymbol,
  mkGYTxOut,
  mkGYTxOutNoDatum,
  pubKeyHashToPlutus,
  redeemerFromPlutusData,
  tokenNameToPlutus,
  validatorToScript,
  valueFromLovelace,
  valueSingleton,
 )

import Data.List.NonEmpty qualified as NE

type BuyerPubkeyHash = GYPubKeyHash

mkMarketplaceInput :: GYValidator 'PlutusV2 -> Maybe GYTxOutRef -> MarketplaceInfo -> MarketplaceAction -> GYTxIn 'PlutusV2
mkMarketplaceInput validator mRefScript info action =
  let
    witness = maybe (GYInScript validator) (\ref -> GYInReference ref (validatorToScript validator)) mRefScript
   in
    GYTxIn
      { gyTxInTxOutRef = mktInfoTxOutRef info
      , gyTxInWitness = GYTxInWitnessScript witness (datumFromPlutusData $ marketplaceInfoToDatum info) (redeemerFromPlutusData action)
      }

mkCarbontokenValue :: MarketplaceInfo -> Integer -> GYValue
mkCarbontokenValue MarketplaceInfo {..} = valueSingleton (GYToken mktInfoCarbonPolicyId mktInfoCarbonAssetName)

buy ::
  -- | The network Id
  GYNetworkId ->
  -- | The marketplace info
  MarketplaceInfo ->
  -- | The oracle info
  OracleInfo ->
  -- | The buyer's PubKeyHash
  BuyerPubkeyHash ->
  -- | Optional marketplace reference script UtxoRef
  Maybe GYTxOutRef ->
  -- | The marketplace parameters
  MarketplaceParams ->
  -- | The scripts
  Scripts ->
  GYTxSkeleton 'PlutusV2
buy nid info@MarketplaceInfo {..} OracleInfo {..} buyerPubKeyHash mMarketplaceRefScript mktPlaceParams scripts =
  let mktPlaceValidator = marketplaceValidator mktPlaceParams scripts
      escrowAddress = addressFromPubKeyHash nid (mktPrmEscrowValidator mktPlaceParams)
      ownerAddress = addressFromPubKeyHash nid mktInfoOwner

      newDatum =
        MarketplaceDatum
          { mktDtmOwner = pubKeyHashToPlutus buyerPubKeyHash
          , mktDtmSalePrice = mktInfoSalePrice
          , mktDtmAssetSymbol = mintingPolicyIdCurrencySymbol mktInfoCarbonPolicyId
          , mktDtmAssetName = tokenNameToPlutus mktInfoCarbonAssetName
          , mktDtmAmount = mktInfoAmount
          , mktDtmIssuer = pubKeyHashToPlutus mktInfoIssuer
          , mktDtmIsSell = 0
          }
   in mustHaveRefInput orcInfoUtxoRef
        <> mustHaveInput (mkMarketplaceInput mktPlaceValidator mMarketplaceRefScript info Marketplace.BUY)
        <> mustHaveOutput (mkGYTxOutNoDatum ownerAddress (valueFromLovelace (mktInfoSalePrice * mktInfoAmount)))
        <> mustHaveOutput (mkGYTxOut mktInfoAddress (mkCarbontokenValue info mktInfoAmount) (datumFromPlutusData newDatum))
        <> mustHaveOutput (mkGYTxOutNoDatum escrowAddress (valueFromLovelace orcInfoRate))
        <> mustBeSignedBy buyerPubKeyHash

partialBuy ::
  -- | The Network Id
  GYNetworkId ->
  -- | The Marketplace Info
  MarketplaceInfo ->
  -- | The Oracle Info
  OracleInfo ->
  -- | The Buyer's PubKeyHash
  BuyerPubkeyHash ->
  -- | The amount to buy
  Integer ->
  -- | Optional marketplace reference script UtxoRef
  Maybe GYTxOutRef ->
  -- | The Marketplace Parameters
  MarketplaceParams ->
  -- | The Scripts
  Scripts ->
  GYTxSkeleton 'PlutusV2
partialBuy nid info@MarketplaceInfo {..} OracleInfo {..} buyerPubKeyHash amount mMarketplaceRefScript mktplaceParams scripts =
  let mktPlaceValidator = marketplaceValidator mktplaceParams scripts
      escrowAddress = addressFromPubKeyHash nid (mktPrmEscrowValidator mktplaceParams)
      ownerAddress = addressFromPubKeyHash nid mktInfoOwner

      newOwnerDatum =
        MarketplaceDatum
          { mktDtmOwner = pubKeyHashToPlutus buyerPubKeyHash
          , mktDtmSalePrice = mktInfoSalePrice
          , mktDtmAssetSymbol = mintingPolicyIdCurrencySymbol mktInfoCarbonPolicyId
          , mktDtmAssetName = tokenNameToPlutus mktInfoCarbonAssetName
          , mktDtmAmount = amount
          , mktDtmIssuer = pubKeyHashToPlutus mktInfoIssuer
          , mktDtmIsSell = 0
          }

      changeMarketplaceDatum =
        MarketplaceDatum
          { mktDtmOwner = pubKeyHashToPlutus mktInfoOwner
          , mktDtmSalePrice = mktInfoSalePrice
          , mktDtmAssetSymbol = mintingPolicyIdCurrencySymbol mktInfoCarbonPolicyId
          , mktDtmAssetName = tokenNameToPlutus mktInfoCarbonAssetName
          , mktDtmAmount = mktInfoAmount - amount
          , mktDtmIssuer = pubKeyHashToPlutus mktInfoIssuer
          , mktDtmIsSell = 1
          }
   in mustHaveRefInput orcInfoUtxoRef
        <> mustHaveInput (mkMarketplaceInput mktPlaceValidator mMarketplaceRefScript info (Marketplace.BUY_PARTIAL amount))
        <> mustHaveOutput (mkGYTxOutNoDatum ownerAddress (valueFromLovelace (mktInfoSalePrice * amount)))
        <> mustHaveOutput (mkGYTxOut mktInfoAddress (mkCarbontokenValue info amount) (datumFromPlutusData newOwnerDatum))
        <> mustHaveOutput (mkGYTxOut mktInfoAddress (mkCarbontokenValue info (mktInfoAmount - amount)) (datumFromPlutusData changeMarketplaceDatum))
        <> mustHaveOutput (mkGYTxOutNoDatum escrowAddress (valueFromLovelace orcInfoRate))
        <> mustBeSignedBy buyerPubKeyHash

sell ::
  -- | The Network Id
  GYNetworkId ->
  -- | The Marketplace Info
  MarketplaceInfo ->
  -- | The Oracle Info
  OracleInfo ->
  -- | Optional marketplace reference script UtxoRef
  Maybe GYTxOutRef ->
  -- | The new sale price
  Integer ->
  -- | The Marketplace Parameters
  MarketplaceParams ->
  -- | The Scripts
  Scripts ->
  GYTxSkeleton 'PlutusV2
sell nid info@MarketplaceInfo {..} OracleInfo {..} mMarketplaceRefScript newSalePrice mktplaceParams scripts =
  let escrowAddress = addressFromPubKeyHash nid (mktPrmEscrowValidator mktplaceParams)
      sellDatum =
        MarketplaceDatum
          { mktDtmOwner = pubKeyHashToPlutus mktInfoOwner
          , mktDtmSalePrice = newSalePrice
          , mktDtmAssetSymbol = mintingPolicyIdCurrencySymbol mktInfoCarbonPolicyId
          , mktDtmAssetName = tokenNameToPlutus mktInfoCarbonAssetName
          , mktDtmAmount = mktInfoAmount
          , mktDtmIssuer = pubKeyHashToPlutus mktInfoIssuer
          , mktDtmIsSell = 1
          }
   in mustHaveRefInput orcInfoUtxoRef
        <> mustHaveInput (mkMarketplaceInput (marketplaceValidator mktplaceParams scripts) mMarketplaceRefScript info (Marketplace.SELL newSalePrice))
        <> mustHaveOutput (mkGYTxOutNoDatum escrowAddress (valueFromLovelace orcInfoRate))
        <> mustHaveOutput (mkGYTxOut mktInfoAddress (mkCarbontokenValue info mktInfoAmount) (datumFromPlutusData sellDatum))
        <> mustBeSignedBy mktInfoOwner

cancel ::
  -- | The Network Id
  GYNetworkId ->
  -- | The Marketplace Info
  MarketplaceInfo ->
  -- | The Oracle Info
  OracleInfo ->
  -- | Optional marketplace reference script UtxoRef
  Maybe GYTxOutRef ->
  -- | The Marketplace Parameters
  MarketplaceParams ->
  -- | The Scripts
  Scripts ->
  GYTxSkeleton 'PlutusV2
cancel nid info@MarketplaceInfo {..} OracleInfo {..} mMarketplaceRefScript mktplaceParams scripts =
  let escrowAddress = addressFromPubKeyHash nid (mktPrmEscrowValidator mktplaceParams)
      cancelDatum =
        MarketplaceDatum
          { mktDtmOwner = pubKeyHashToPlutus mktInfoOwner
          , mktDtmSalePrice = mktInfoSalePrice
          , mktDtmAssetSymbol = mintingPolicyIdCurrencySymbol mktInfoCarbonPolicyId
          , mktDtmAssetName = tokenNameToPlutus mktInfoCarbonAssetName
          , mktDtmAmount = mktInfoAmount
          , mktDtmIssuer = pubKeyHashToPlutus mktInfoIssuer
          , mktDtmIsSell = 0
          }
   in mustHaveRefInput orcInfoUtxoRef
        <> mustHaveInput (mkMarketplaceInput (marketplaceValidator mktplaceParams scripts) mMarketplaceRefScript info Marketplace.CANCEL)
        <> mustHaveOutput (mkGYTxOutNoDatum escrowAddress (valueFromLovelace orcInfoRate))
        <> mustHaveOutput (mkGYTxOut mktInfoAddress (mkCarbontokenValue info mktInfoAmount) (datumFromPlutusData cancelDatum))
        <> mustBeSignedBy mktInfoOwner

merge ::
  -- | The Network Id
  GYNetworkId ->
  -- | NonEmpty Marketplace Infos
  NE.NonEmpty MarketplaceInfo ->
  -- | The Oracle Info
  OracleInfo ->
  -- | Optional marketplace reference script UtxoRef
  Maybe GYTxOutRef ->
  -- | The Escrow PubkeyHash
  GYPubKeyHash ->
  -- | The Marketplace Validator
  GYValidator 'PlutusV2 ->
  GYTxSkeleton 'PlutusV2
merge nid infos OracleInfo {..} mMarketplaceRefScript escrowPubkeyHash mktValidator =
  let info = NE.head infos
      filteredInfos = NE.filter (\m -> mktInfoOwner m == mktInfoOwner info && mktInfoIsSell m == 0 && mktInfoIssuer m == mktInfoIssuer info) infos
      inputs = foldMap (\info -> mustHaveInput $ mkMarketplaceInput mktValidator mMarketplaceRefScript info Marketplace.MERGE) filteredInfos
      escrowAddress = addressFromPubKeyHash nid escrowPubkeyHash
      mergedAmt = sum $ mktInfoAmount <$> NE.toList infos
      newDatum =
        MarketplaceDatum
          { mktDtmOwner = pubKeyHashToPlutus $ mktInfoOwner info
          , mktDtmSalePrice = orcInfoRate
          , mktDtmAssetSymbol = mintingPolicyIdCurrencySymbol $ mktInfoCarbonPolicyId info
          , mktDtmAssetName = tokenNameToPlutus $ mktInfoCarbonAssetName info
          , mktDtmAmount = mergedAmt
          , mktDtmIssuer = pubKeyHashToPlutus $ mktInfoIssuer info
          , mktDtmIsSell = 0
          }
   in mustHaveRefInput orcInfoUtxoRef
        <> inputs
        <> mustHaveOutput (mkGYTxOutNoDatum escrowAddress (valueFromLovelace orcInfoRate))
        <> mustHaveOutput (mkGYTxOut (mktInfoAddress info) (mkCarbontokenValue info mergedAmt) (datumFromPlutusData newDatum))
        <> mustBeSignedBy (mktInfoOwner info)

merge' ::
  -- | The Network Id
  GYNetworkId ->
  -- | NonEmpty Marketplace Infos
  NE.NonEmpty MarketplaceInfo ->
  -- | The Oracle Info
  OracleInfo ->
  Integer ->
  Integer ->
  Integer ->
  -- | Optional marketplace reference script UtxoRef
  Maybe GYTxOutRef ->
  MarketplaceParams ->
  -- | The Scripts
  Scripts ->
  GYTxSkeleton 'PlutusV2
merge' nid infos OracleInfo {..} newPrice newAmount isSell mMarketplaceRefScript mktplaceParams scripts =
  let info = NE.head infos
      mktValidator = marketplaceValidator mktplaceParams scripts
      filteredInfos = NE.filter (\m -> mktInfoOwner m == mktInfoOwner info && mktInfoIsSell m == 0 && mktInfoIssuer m == mktInfoIssuer info) infos
      inputs = foldMap (\info -> mustHaveInput $ mkMarketplaceInput mktValidator mMarketplaceRefScript info Marketplace.MERGE) filteredInfos
      escrowAddress = addressFromPubKeyHash nid (mktPrmEscrowValidator mktplaceParams)
      mergedAmt = sum $ mktInfoAmount <$> NE.toList infos
      newDatum =
        MarketplaceDatum
          { mktDtmOwner = pubKeyHashToPlutus $ mktInfoOwner info
          , mktDtmSalePrice = newPrice
          , mktDtmAssetSymbol = mintingPolicyIdCurrencySymbol $ mktInfoCarbonPolicyId info
          , mktDtmAssetName = tokenNameToPlutus $ mktInfoCarbonAssetName info
          , mktDtmAmount = newAmount
          , mktDtmIssuer = pubKeyHashToPlutus $ mktInfoIssuer info
          , mktDtmIsSell = isSell
          }
      oldDatum =
        MarketplaceDatum
          { mktDtmOwner = pubKeyHashToPlutus $ mktInfoOwner info
          , mktDtmSalePrice = mktInfoSalePrice info
          , mktDtmAssetSymbol = mintingPolicyIdCurrencySymbol $ mktInfoCarbonPolicyId info
          , mktDtmAssetName = tokenNameToPlutus $ mktInfoCarbonAssetName info
          , mktDtmAmount = mergedAmt - newAmount
          , mktDtmIssuer = pubKeyHashToPlutus $ mktInfoIssuer info
          , mktDtmIsSell = 0
          }
   in if newAmount == mergedAmt
        then
          mustHaveRefInput orcInfoUtxoRef
            <> inputs
            <> mustHaveOutput (mkGYTxOutNoDatum escrowAddress (valueFromLovelace orcInfoRate))
            <> mustHaveOutput (mkGYTxOut (mktInfoAddress info) (mkCarbontokenValue info mergedAmt) (datumFromPlutusData newDatum))
            <> mustBeSignedBy (mktInfoOwner info)
        else
          mustHaveRefInput orcInfoUtxoRef
            <> inputs
            <> mustHaveOutput (mkGYTxOutNoDatum escrowAddress (valueFromLovelace orcInfoRate))
            <> mustHaveOutput (mkGYTxOut (mktInfoAddress info) (mkCarbontokenValue info newAmount) (datumFromPlutusData newDatum))
            <> mustHaveOutput (mkGYTxOut (mktInfoAddress info) (mkCarbontokenValue info (mergedAmt - newAmount)) (datumFromPlutusData oldDatum))
            <> mustBeSignedBy (mktInfoOwner info)
