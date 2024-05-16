module EA.CLI.Marketplace.Run (runMarketplaceCommand) where

import EA (EAAppEnv (eaAppEnvGYNetworkId, eaAppEnvMarketplaceBackdoorPubKeyHash, eaAppEnvMarketplaceEscrowPubKeyHash, eaAppEnvMarketplaceVersion, eaAppEnvOracleNftMintingPolicyId, eaAppEnvOracleNftTokenName, eaAppEnvOracleOperatorPubKeyHash, eaAppEnvScripts), eaLiftMaybe, eaSubmitTx, initEAApp, runEAApp)
import EA.CLI.Helper (fetchCoreCfg, fetchRootKeyFilePath)
import EA.CLI.Marketplace.Command (MarketplaceCommand (MarketplaceDeployScript), MarketplaceDeployCommand (MarketplaceDeployCommand))
import EA.Script (oracleValidator)
import EA.Script.Marketplace (MarketplaceParams (..))
import EA.Tx.Changeblock.Marketplace (deployScript)
import EA.Wallet (eaGetCollateralFromInternalWallet, eaGetInternalAddresses, eaSelectOref)
import GeniusYield.GYConfig (withCfgProviders)
import GeniusYield.Imports (printf)
import GeniusYield.TxBuilder (runGYTxMonadNode)
import GeniusYield.Types (GYAssetClass (GYToken), unsafeAddressFromText, validatorHash)
import Internal.Wallet qualified as Wallet

runMarketplaceCommand :: MarketplaceCommand -> IO ()
runMarketplaceCommand (MarketplaceDeployScript (MarketplaceDeployCommand dplMktplaceAddr)) = do
  printf "Deploying Marketplace Script to Address: %s" dplMktplaceAddr
  coreCfg <- fetchCoreCfg
  providers <- withCfgProviders coreCfg "cli" return
  rootKeyFilePath <- fetchRootKeyFilePath

  env <- initEAApp coreCfg providers rootKeyFilePath 1
  internalAddrPairs <- runEAApp env $ eaGetInternalAddresses False
  oracleNftPolicyId <- runEAApp env $ asks eaAppEnvOracleNftMintingPolicyId >>= eaLiftMaybe "No Oracle NFT Policy Id"
  oracleNftTknName <- runEAApp env $ asks eaAppEnvOracleNftTokenName >>= eaLiftMaybe "No Oracle NFT Token Name"
  escrowPubkeyHash <- runEAApp env $ asks eaAppEnvMarketplaceEscrowPubKeyHash
  backdoorPubkeyHash <- runEAApp env $ asks eaAppEnvMarketplaceBackdoorPubKeyHash

  version <- runEAApp env $ asks eaAppEnvMarketplaceVersion
  networkId <- runEAApp env $ asks eaAppEnvGYNetworkId

  -- Get the collateral address and its signing key.
  (collateral, colKey) <- runEAApp env $ eaGetCollateralFromInternalWallet >>= eaLiftMaybe "No collateral found"

  (addr, key, _oref) <- runEAApp env $ eaSelectOref internalAddrPairs (\r -> collateral /= Just (r, True)) >>= eaLiftMaybe "No UTxO found"

  let scripts = eaAppEnvScripts env
      oracleValidatorHash = validatorHash $ oracleValidator (GYToken oracleNftPolicyId oracleNftTknName) (eaAppEnvOracleOperatorPubKeyHash env) scripts
      marketplaceParams =
        MarketplaceParams
          { mktPrmOracleValidator = oracleValidatorHash
          , mktPrmEscrowValidator = escrowPubkeyHash
          , mktPrmVersion = version
          , mktPrmOracleSymbol = oracleNftPolicyId
          , mktPrmOracleTokenName = oracleNftTknName
          , mktPrmBackdoor = backdoorPubkeyHash
          }
      skeleton = deployScript (unsafeAddressFromText dplMktplaceAddr) marketplaceParams scripts

  txBody <-
    liftIO $
      runGYTxMonadNode networkId providers [addr] addr collateral (return skeleton)

  gyTxId <- runEAApp env $ eaSubmitTx $ Wallet.signTx txBody [key, colKey]

  printf "\n \n export MARKETPLACE_REF_SCRIPT_UTXO=%s#0 \n" gyTxId