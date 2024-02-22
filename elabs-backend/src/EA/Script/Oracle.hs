{-# LANGUAGE TemplateHaskell #-}

module EA.Script.Oracle (OracleDatum (..), OracleScriptParams (..), OracleAction (..), OracleInfo (..), oracleDatumToInfo) where

import GeniusYield.Types
import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V1.Value (AssetClass)
import PlutusTx qualified
import PlutusTx.Prelude qualified as PlutusTx

data OracleDatum = OracleDatum
  { orcDtmRate :: PlutusTx.Integer
  }

PlutusTx.unstableMakeIsData ''OracleDatum

data OracleScriptParams = OracleScriptParams
  { orcScriptPrmNft :: AssetClass
  , orcScriptPrmOperator :: PubKeyHash
  }

PlutusTx.unstableMakeIsData ''OracleScriptParams

data OracleAction
  = Update
  | Delete

PlutusTx.makeIsDataIndexed ''OracleAction [('Update, 0), ('Delete, 1)]

data OracleInfo = OracleInfo
  { orcInfoUtxoRef :: GYTxOutRef
  , orcInfoAddress :: GYAddress
  , orcInfoValue :: GYValue
  , orcInfoRate :: Integer
  }
  deriving stock (Show)

oracleDatumToInfo ::
  GYTxOutRef ->
  GYValue ->
  GYAddress ->
  OracleDatum ->
  Either String OracleInfo
oracleDatumToInfo oref val addr datum = do
  return
    OracleInfo
      { orcInfoUtxoRef = oref
      , orcInfoAddress = addr
      , orcInfoValue = val
      , orcInfoRate = orcDtmRate datum
      }
