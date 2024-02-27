module EA.Tx.Changeblock.Oracle (createOracle, updateOracle, deleteOracle) where

import EA.Script.Oracle (OracleDatum (..), OracleInfo (..))
import EA.Script.Oracle qualified as Oracle
import GeniusYield.TxBuilder (GYTxSkeleton, mustBeSignedBy, mustHaveInput, mustHaveOutput, mustMint)
import GeniusYield.Types

type OracleAddress = GYAddress

createOracle ::
  -- | The Oracle Rate
  Natural ->
  --  | The UtxoRef to consume for minting
  GYTxOutRef ->
  -- | The Oracle Address
  OracleAddress ->
  -- | The Oracle NFT Token Name
  GYTokenName ->
  -- | The Oracle NFT Minting Policy
  GYMintingPolicy 'PlutusV2 ->
  -- Tx Skeleton
  GYTxSkeleton 'PlutusV2
createOracle rate utxoRef orcAddr orcleNftTokenName oracleNftMintingPolicy =
  let nftMintingPolicyId = mintingPolicyId oracleNftMintingPolicy
      nftValue = valueSingleton (GYToken nftMintingPolicyId orcleNftTokenName) 1
   in mustMint (GYMintScript oracleNftMintingPolicy) unitRedeemer orcleNftTokenName 1
        <> mustHaveInput (GYTxIn utxoRef GYTxInWitnessKey)
        <> mustHaveOutput (mkGYTxOut orcAddr nftValue (datumFromPlutusData $ OracleDatum $ toInteger rate))

updateOracle ::
  -- | New Rate
  Natural ->
  -- | The Oracle Info To update
  OracleInfo ->
  -- | Oracle NFT Minting Policy Id
  GYMintingPolicyId ->
  -- | Oracle NFT Token Name
  GYTokenName ->
  -- | Operator PubKeyHash
  GYPubKeyHash ->
  -- | Oracle Validator
  GYValidator 'PlutusV2 ->
  -- Tx Skeleton
  GYTxSkeleton 'PlutusV2
updateOracle newRate OracleInfo {..} orcNftPolicyId orcNftTokenName operatorPubKeyHash oracleValidator =
  let nftValue = valueSingleton (GYToken orcNftPolicyId orcNftTokenName) 1
      -- Todo: Use Ref script if possible
      witness = GYTxInWitnessScript (GYInScript oracleValidator) (datumFromPlutusData $ OracleDatum $ toInteger orcInfoRate) (redeemerFromPlutusData Oracle.Update)
      orcInput = GYTxIn orcInfoUtxoRef witness
   in mustHaveInput orcInput
        <> mustHaveOutput (mkGYTxOut orcInfoAddress nftValue (datumFromPlutusData $ OracleDatum $ toInteger newRate))
        <> mustBeSignedBy operatorPubKeyHash

deleteOracle ::
  -- The Oracle Info To delete
  OracleInfo ->
  -- The Operator PubKeyHash
  GYPubKeyHash ->
  -- The Oracle Validator
  GYValidator 'PlutusV2 ->
  -- Tx Skeleton
  GYTxSkeleton 'PlutusV2
deleteOracle OracleInfo {..} operatorPubKeyHash oracleValidator =
  let witness = GYTxInWitnessScript (GYInScript oracleValidator) (datumFromPlutusData $ OracleDatum $ toInteger orcInfoRate) (redeemerFromPlutusData Oracle.Delete)
      orcInput = GYTxIn orcInfoUtxoRef witness
   in mustHaveInput orcInput
        <> mustBeSignedBy operatorPubKeyHash
