module EA.CLI.Oracle.Run (
  runOracleCommand,
) where

import EA (EAAppEnv (eaAppEnvGYNetworkId, eaAppEnvOracleOperatorPubKeyHash, eaAppEnvScripts), eaLiftMaybe, eaSubmitTx, initEAApp, runEAApp)
import EA.Api.Ctx (runSkeletonF)
import EA.CLI.Helper (fetchCoreCfg, fetchRootKeyFilePath)
import EA.CLI.Oracle.Command (OracleCommand (..), OracleCreateCommand (..))
import EA.Script (nftMintingPolicy, oracleValidator)
import EA.Tx.Nexchange.Oracle (createOracle)
import EA.Wallet (eaGetCollateralFromInternalWallet, eaGetInternalAddresses, eaSelectOref)
import GeniusYield.GYConfig (withCfgProviders)
import GeniusYield.Imports (printf)
import GeniusYield.Types (GYAssetClass (GYToken), addressFromValidator, mintingPolicyId, unsafeTokenNameFromHex)
import Internal.Wallet qualified as Wallet

runOracleCommand :: OracleCommand -> IO ()
runOracleCommand (CreateOracle (OracleCreateCommand rate oracleAssetName)) = do
  printf "Creating Oracle ..."
  coreCfg <- fetchCoreCfg
  providers <- withCfgProviders coreCfg "cli" return
  rootKeyFilePath <- fetchRootKeyFilePath

  env <- initEAApp coreCfg providers rootKeyFilePath 1
  internalAddrPairs <- runEAApp env $ eaGetInternalAddresses False

  -- Get the collateral address and its signing key.
  (collateral, colKey) <- runEAApp env $ eaGetCollateralFromInternalWallet >>= eaLiftMaybe "No collateral found"

  (addr, key, oref) <- runEAApp env $ eaSelectOref internalAddrPairs (\r -> collateral /= Just (r, True)) >>= eaLiftMaybe "No UTxO found"

  -- TODO: User proper policyId for Oracle NFT
  let operatorPubkeyHash = eaAppEnvOracleOperatorPubKeyHash env
      scripts = eaAppEnvScripts env
      networkId = eaAppEnvGYNetworkId env
      orcNftPolicy = nftMintingPolicy oref scripts
      oracleNftAsset = mintingPolicyId orcNftPolicy
      orcTokenName = unsafeTokenNameFromHex oracleAssetName
      orcAssetClass = GYToken oracleNftAsset orcTokenName
      orcValidator = oracleValidator orcAssetClass operatorPubkeyHash scripts
      orcAddress = addressFromValidator networkId orcValidator
      skeleton = createOracle (fromIntegral rate) oref orcAddress orcTokenName orcNftPolicy

  txBody <-
    liftIO $
      runSkeletonF networkId providers [addr] addr collateral (return skeleton)

  gyTxId <- runEAApp env $ eaSubmitTx $ Wallet.signTx txBody [key, colKey]
  printf "\n Oracle created with TxId: %s \n " gyTxId
  printf "\n Operator pubkeyHash: %s \n Operator Address: %s \n" operatorPubkeyHash addr
  printf "\n Oracle NFT Asset: %s \n" orcAssetClass
  printf "\n Oracle Address: %s \n" orcAddress
  printf "\n \n export ORACLE_UTXO_REF=%s#0 \n" gyTxId