module EA.Tx.Changeblock.Marketplace (buy, partialBuy, sell, cancel, merge, adjustOrders, deployScript, withdrawCarbonToken) where

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
import GeniusYield.Types

import Data.List.NonEmpty qualified as NE

type BuyerPubkeyHash = GYPaymentKeyHash

mkMarketplaceInput :: GYValidator 'PlutusV2 -> Maybe GYTxOutRef -> MarketplaceInfo -> MarketplaceAction -> GYTxIn 'PlutusV2
mkMarketplaceInput validator mRefScript info action =
  let
    witness = maybe (GYInScript validator) (\ref -> GYInReference ref (validatorToScript validator)) mRefScript
   in
    GYTxIn
      { gyTxInTxOutRef = mktInfoTxOutRef info
      , gyTxInWitness = GYTxInWitnessScript witness (datumFromPlutusData $ marketplaceInfoToDatum info) (redeemerFromPlutusData action)
      }

mkEscrowOutput :: GYAddress -> Integer -> Maybe Double -> Integer -> Integer -> GYTxOut 'PlutusV2
mkEscrowOutput addr oracleRate mFee amt rate =
  let amt = max oracleRate $ round calcFee
   in mkGYTxOutNoDatum addr (valueFromLovelace amt)
  where
    calcFee = case mFee of
      Just fee -> fee * fromInteger (amt * rate)
      Nothing -> 0

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
      escrowAddress = addressFromPaymentKeyHash nid (mktPrmEscrowValidator mktPlaceParams)
      ownerAddress = addressFromPaymentKeyHash nid mktInfoOwner
      fee = Just 0.0215 -- Buy fee is 2.15%
      newDatum =
        MarketplaceDatum
          { mktDtmOwner = paymentKeyHashToPlutus buyerPubKeyHash
          , mktDtmSalePrice = mktInfoSalePrice
          , mktDtmAssetSymbol = mintingPolicyIdToCurrencySymbol mktInfoCarbonPolicyId
          , mktDtmAssetName = tokenNameToPlutus mktInfoCarbonAssetName
          , mktDtmAmount = mktInfoAmount
          , mktDtmIssuer = paymentKeyHashToPlutus mktInfoIssuer
          , mktDtmIsSell = 0
          }
   in mustHaveRefInput orcInfoUtxoRef
        <> mustHaveInput (mkMarketplaceInput mktPlaceValidator mMarketplaceRefScript info Marketplace.BUY)
        <> mustHaveOutput (mkGYTxOutNoDatum ownerAddress (valueFromLovelace (mktInfoSalePrice * mktInfoAmount)))
        <> mustHaveOutput (mkGYTxOut mktInfoAddress (mkCarbontokenValue info mktInfoAmount) (datumFromPlutusData newDatum))
        <> mustHaveOutput (mkEscrowOutput escrowAddress orcInfoRate fee mktInfoAmount mktInfoSalePrice)
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
      escrowAddress = addressFromPaymentKeyHash nid (mktPrmEscrowValidator mktplaceParams)
      ownerAddress = addressFromPaymentKeyHash nid mktInfoOwner
      fee = Just 0.0215 -- Buy fee is 2.15%
      newOwnerDatum =
        MarketplaceDatum
          { mktDtmOwner = paymentKeyHashToPlutus buyerPubKeyHash
          , mktDtmSalePrice = mktInfoSalePrice
          , mktDtmAssetSymbol = mintingPolicyIdToCurrencySymbol mktInfoCarbonPolicyId
          , mktDtmAssetName = tokenNameToPlutus mktInfoCarbonAssetName
          , mktDtmAmount = amount
          , mktDtmIssuer = paymentKeyHashToPlutus mktInfoIssuer
          , mktDtmIsSell = 0
          }

      changeMarketplaceDatum =
        MarketplaceDatum
          { mktDtmOwner = paymentKeyHashToPlutus mktInfoOwner
          , mktDtmSalePrice = mktInfoSalePrice
          , mktDtmAssetSymbol = mintingPolicyIdToCurrencySymbol mktInfoCarbonPolicyId
          , mktDtmAssetName = tokenNameToPlutus mktInfoCarbonAssetName
          , mktDtmAmount = mktInfoAmount - amount
          , mktDtmIssuer = paymentKeyHashToPlutus mktInfoIssuer
          , mktDtmIsSell = 1
          }
   in mustHaveRefInput orcInfoUtxoRef
        <> mustHaveInput (mkMarketplaceInput mktPlaceValidator mMarketplaceRefScript info (Marketplace.BUY_PARTIAL amount))
        <> mustHaveOutput (mkGYTxOutNoDatum ownerAddress (valueFromLovelace (mktInfoSalePrice * amount)))
        <> mustHaveOutput (mkGYTxOut mktInfoAddress (mkCarbontokenValue info amount) (datumFromPlutusData newOwnerDatum))
        <> mustHaveOutput (mkGYTxOut mktInfoAddress (mkCarbontokenValue info (mktInfoAmount - amount)) (datumFromPlutusData changeMarketplaceDatum))
        <> mustHaveOutput (mkEscrowOutput escrowAddress orcInfoRate fee amount mktInfoSalePrice)
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
  let escrowAddress = addressFromPaymentKeyHash nid (mktPrmEscrowValidator mktplaceParams)
      sellDatum =
        MarketplaceDatum
          { mktDtmOwner = paymentKeyHashToPlutus mktInfoOwner
          , mktDtmSalePrice = newSalePrice
          , mktDtmAssetSymbol = mintingPolicyIdToCurrencySymbol mktInfoCarbonPolicyId
          , mktDtmAssetName = tokenNameToPlutus mktInfoCarbonAssetName
          , mktDtmAmount = mktInfoAmount
          , mktDtmIssuer = paymentKeyHashToPlutus mktInfoIssuer
          , mktDtmIsSell = 1
          }
      fee = Just 0.0315 -- Sell fee is 3.15%
   in mustHaveRefInput orcInfoUtxoRef
        <> mustHaveInput (mkMarketplaceInput (marketplaceValidator mktplaceParams scripts) mMarketplaceRefScript info (Marketplace.SELL newSalePrice))
        <> mustHaveOutput (mkEscrowOutput escrowAddress orcInfoRate fee mktInfoAmount newSalePrice)
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
  let escrowAddress = addressFromPaymentKeyHash nid (mktPrmEscrowValidator mktplaceParams)
      cancelDatum =
        MarketplaceDatum
          { mktDtmOwner = paymentKeyHashToPlutus mktInfoOwner
          , mktDtmSalePrice = mktInfoSalePrice
          , mktDtmAssetSymbol = mintingPolicyIdToCurrencySymbol mktInfoCarbonPolicyId
          , mktDtmAssetName = tokenNameToPlutus mktInfoCarbonAssetName
          , mktDtmAmount = mktInfoAmount
          , mktDtmIssuer = paymentKeyHashToPlutus mktInfoIssuer
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
  GYPaymentKeyHash ->
  -- | The Marketplace Validator
  GYValidator 'PlutusV2 ->
  GYTxSkeleton 'PlutusV2
merge nid infos OracleInfo {..} mMarketplaceRefScript escrowPubkeyHash mktValidator =
  let info = NE.head infos
      filteredInfos = NE.filter (\m -> mktInfoOwner m == mktInfoOwner info && mktInfoIsSell m == Marketplace.M_SELL && mktInfoIssuer m == mktInfoIssuer info) infos
      inputs = foldMap (\info -> mustHaveInput $ mkMarketplaceInput mktValidator mMarketplaceRefScript info Marketplace.MERGE) filteredInfos
      escrowAddress = addressFromPaymentKeyHash nid escrowPubkeyHash
      mergedAmt = sum $ mktInfoAmount <$> NE.toList infos
      newDatum =
        MarketplaceDatum
          { mktDtmOwner = paymentKeyHashToPlutus $ mktInfoOwner info
          , mktDtmSalePrice = orcInfoRate
          , mktDtmAssetSymbol = mintingPolicyIdToCurrencySymbol $ mktInfoCarbonPolicyId info
          , mktDtmAssetName = tokenNameToPlutus $ mktInfoCarbonAssetName info
          , mktDtmAmount = mergedAmt
          , mktDtmIssuer = paymentKeyHashToPlutus $ mktInfoIssuer info
          , mktDtmIsSell = 0
          }
   in mustHaveRefInput orcInfoUtxoRef
        <> inputs
        <> mustHaveOutput (mkGYTxOutNoDatum escrowAddress (valueFromLovelace orcInfoRate))
        <> mustHaveOutput (mkGYTxOut (mktInfoAddress info) (mkCarbontokenValue info mergedAmt) (datumFromPlutusData newDatum))
        <> mustBeSignedBy (mktInfoOwner info)

type NewPrice = Integer
type NewAmount = Integer
type NewSellType = Marketplace.MarketplaceOrderType

adjustOrders ::
  GYNetworkId ->
  MarketplaceInfo ->
  OracleInfo ->
  Maybe GYTxOutRef ->
  NewPrice ->
  NewAmount ->
  NewSellType ->
  MarketplaceParams ->
  Scripts ->
  GYTxSkeleton 'PlutusV2
adjustOrders nid info OracleInfo {..} mMarketplaceRefScript newPrice newAmount newSaleInfo marketplaceParams scripts =
  let escrowAddress = addressFromPaymentKeyHash nid (mktPrmEscrowValidator marketplaceParams)
      mktValidator = marketplaceValidator marketplaceParams scripts
      oldAmt = mktInfoAmount info - newAmount
      -- Fee is 3.15% for sell and 2.15% for buy
      fee = if newSaleInfo == Marketplace.M_SELL then Just 0.0315 else Just 0.0215
      oldDatum =
        MarketplaceDatum
          { mktDtmOwner = paymentKeyHashToPlutus $ mktInfoOwner info
          , mktDtmSalePrice = mktInfoSalePrice info
          , mktDtmAssetSymbol = mintingPolicyIdToCurrencySymbol $ mktInfoCarbonPolicyId info
          , mktDtmAssetName = tokenNameToPlutus $ mktInfoCarbonAssetName info
          , mktDtmAmount = oldAmt
          , mktDtmIssuer = paymentKeyHashToPlutus $ mktInfoIssuer info
          , mktDtmIsSell = toInteger $ fromEnum $ mktInfoIsSell info
          }

      newDatum =
        MarketplaceDatum
          { mktDtmOwner = paymentKeyHashToPlutus $ mktInfoOwner info
          , mktDtmSalePrice = newPrice
          , mktDtmAssetSymbol = mintingPolicyIdToCurrencySymbol $ mktInfoCarbonPolicyId info
          , mktDtmAssetName = tokenNameToPlutus $ mktInfoCarbonAssetName info
          , mktDtmAmount = newAmount
          , mktDtmIssuer = paymentKeyHashToPlutus $ mktInfoIssuer info
          , mktDtmIsSell = toInteger $ fromEnum newSaleInfo
          }
   in mustHaveRefInput orcInfoUtxoRef
        <> mustHaveInput (mkMarketplaceInput mktValidator mMarketplaceRefScript info Marketplace.MERGE)
        <> mustHaveOutput (mkGYTxOut (mktInfoAddress info) (mkCarbontokenValue info oldAmt) (datumFromPlutusData oldDatum))
        <> mustHaveOutput (mkGYTxOut (mktInfoAddress info) (mkCarbontokenValue info newAmount) (datumFromPlutusData newDatum))
        <> mustHaveOutput (mkEscrowOutput escrowAddress orcInfoRate fee newAmount newPrice)
        <> mustBeSignedBy (mktInfoOwner info)

deployScript :: GYAddress -> MarketplaceParams -> Scripts -> GYTxSkeleton 'PlutusV2
deployScript toAddr marketplaceParams scripts =
  let mktValidator = marketplaceValidator marketplaceParams scripts
      out = GYTxOut toAddr (valueFromLovelace 100) Nothing (Just $ validatorToScript mktValidator)
   in mustHaveOutput out

withdrawCarbonToken :: MarketplaceParams -> GYAddress -> GYAssetClass -> Scripts -> [(GYAddress, MarketplaceInfo)] -> Natural -> GYTxSkeleton 'PlutusV2
withdrawCarbonToken mktplaceParams tokenRecvAddr carbonAsset scripts orderInfos withdrawAmt =
  let validOrders = sortOn (\(_, m) -> mktInfoSalePrice m) $ filter canPickMktInfo orderInfos
   in foldTx backdoorTx withdrawAmt validOrders <> withdrawTx
  where
    backdoorTx = mustBeSignedBy (mktPrmBackdoor mktplaceParams)
    withdrawTx = mustHaveOutput $ mkGYTxOutNoDatum tokenRecvAddr $ valueSingleton carbonAsset (toInteger withdrawAmt)
    mktPlaceValidator = marketplaceValidator mktplaceParams scripts

    foldTx :: GYTxSkeleton 'PlutusV2 -> Natural -> [(GYAddress, MarketplaceInfo)] -> GYTxSkeleton 'PlutusV2
    foldTx tx 0 _ = tx
    foldTx tx amt ((ownerAddr, mInfo@MarketplaceInfo {..}) : infos) =
      let usedAmt = min amt $ fromInteger mktInfoAmount
          finalTx = tx <> prepareTx mInfo ownerAddr (toInteger usedAmt)
       in foldTx finalTx (amt - usedAmt) infos
    foldTx tx _ [] = tx

    canPickMktInfo :: (GYAddress, MarketplaceInfo) -> Bool
    canPickMktInfo (_, MarketplaceInfo {..}) = mktInfoIsSell == Marketplace.M_SELL && carbonAsset == GYToken mktInfoCarbonPolicyId mktInfoCarbonAssetName

    prepareTx :: MarketplaceInfo -> GYAddress -> Integer -> GYTxSkeleton 'PlutusV2
    prepareTx mktInfo@MarketplaceInfo {..} ownerAddress usedAmt =
      let changeAmt = mktInfoAmount - usedAmt
          outSellTx = mustHaveOutput $ mkGYTxOutNoDatum ownerAddress (valueFromLovelace (mktInfoSalePrice * usedAmt))
          inTx = mustHaveInput $ mkMarketplaceInput mktPlaceValidator Nothing mktInfo Marketplace.MERGE
          newDatum =
            MarketplaceDatum
              { mktDtmOwner = paymentKeyHashToPlutus mktInfoOwner
              , mktDtmSalePrice = mktInfoSalePrice
              , mktDtmAssetSymbol = mintingPolicyIdToCurrencySymbol mktInfoCarbonPolicyId
              , mktDtmAssetName = tokenNameToPlutus mktInfoCarbonAssetName
              , mktDtmAmount = changeAmt
              , mktDtmIssuer = paymentKeyHashToPlutus mktInfoIssuer
              , mktDtmIsSell = 1
              }
       in if changeAmt > 0
            then inTx <> outSellTx <> mustHaveOutput (mkGYTxOut mktInfoAddress (mkCarbontokenValue mktInfo changeAmt) (datumFromPlutusData newDatum))
            else inTx <> outSellTx