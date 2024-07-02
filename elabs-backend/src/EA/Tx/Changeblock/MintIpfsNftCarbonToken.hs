module EA.Tx.Changeblock.MintIpfsNftCarbonToken (mintIpfsNftCarbonToken) where

import EA.Script (Scripts (..), carbonNftMintingPolicy, carbonTokenMintingPolicy)
import EA.Script.Marketplace (MarketplaceDatum (..))
import GeniusYield.TxBuilder (GYTxSkeleton, mustHaveInput, mustHaveOutput, mustMint)
import GeniusYield.Types

type MarketPlaceSalePrice = Integer

type MarketPlaceAddress = GYAddress

mintIpfsNftCarbonToken ::
  -- | The UTXO to consume
  GYTxOutRef ->
  -- | The Marketplace Address
  MarketPlaceAddress ->
  -- | Owner Address
  GYAddress ->
  -- | owner Pubkey hash
  GYPaymentKeyHash ->
  -- | Issuer Pubkey hash
  GYPaymentKeyHash ->
  -- | TokenName generated with combination of IPFS hash
  GYTokenName ->
  -- | Marketplace Sale Price
  MarketPlaceSalePrice ->
  -- | Amount of token to mint
  Integer ->
  -- | The EA Scripts
  Scripts ->
  GYTxSkeleton 'PlutusV2
mintIpfsNftCarbonToken oref marketplaceAddr ownerAddr ownerPkh issuerPkh tn mktSalePrice qty script =
  let carbonNftPolicy = carbonNftMintingPolicy oref tn script
      carbonTokenPolicy = carbonTokenMintingPolicy tn script
      carbonToken = GYToken (mintingPolicyId carbonTokenPolicy) tn
      carbonNftToken = GYToken (mintingPolicyId carbonNftPolicy) tn

      marketPlaceDatum =
        MarketplaceDatum
          { mktDtmOwner = paymentKeyHashToPlutus ownerPkh
          , mktDtmIssuer = paymentKeyHashToPlutus issuerPkh
          , mktDtmIsSell = 1
          , mktDtmSalePrice = mktSalePrice
          , mktDtmAssetSymbol = mintingPolicyCurrencySymbol carbonTokenPolicy
          , mktDtmAssetName = tokenNameToPlutus tn
          , mktDtmAmount = qty
          }
   in mustMint (GYMintScript carbonNftPolicy) unitRedeemer tn 1
        <> mustMint (GYMintScript carbonTokenPolicy) unitRedeemer tn qty
        <> mustHaveInput (GYTxIn oref GYTxInWitnessKey)
        <> mustHaveOutput (mkGYTxOut marketplaceAddr (valueSingleton carbonToken qty) (datumFromPlutusData marketPlaceDatum))
        <> mustHaveOutput (mkGYTxOut ownerAddr (valueSingleton carbonNftToken 1) (datumFromPlutusData ()))
