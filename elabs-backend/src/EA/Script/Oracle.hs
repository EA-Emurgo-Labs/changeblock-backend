{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module EA.Script.Oracle (OracleDatum (..), OracleScriptParams (..), OracleAction (..), OracleInfo (..), oracleDatumToInfo, utxoToOracleInfo, oracleNftAsset) where

import GeniusYield.TxBuilder (utxoDatumPure)
import GeniusYield.Types
import PlutusLedgerApi.V1.Value (AssetClass)
import PlutusLedgerApi.V2 (PubKeyHash)
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

oracleDatumToInfo :: GYTxOutRef -> GYValue -> GYAddress -> OracleDatum -> OracleInfo
oracleDatumToInfo utxoRef value address (OracleDatum rate) =
  OracleInfo
    { orcInfoUtxoRef = utxoRef
    , orcInfoAddress = address
    , orcInfoValue = value
    , orcInfoRate = rate
    }

utxoToOracleInfo :: (GYUTxO, Maybe GYDatum) -> Either String OracleInfo
utxoToOracleInfo t@(utxo, _) = do
  (addr, value, dtm) <-
    either (Left . show) Right $ utxoDatumPure @OracleDatum t

  return $ oracleDatumToInfo (utxoRef utxo) value addr dtm

oracleNftAsset :: OracleInfo -> Maybe GYAssetClass
oracleNftAsset OracleInfo {..} = listToMaybe $ map fst $ filter (\(a, n) -> a /= "" && n == 1) $ valueToList orcInfoValue
