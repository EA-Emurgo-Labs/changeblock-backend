module EA.CLI.Marketplace.Run (runMarketplaceCommand) where

import EA (EAAppEnv (eaAppEnvGYNetworkId, eaAppEnvMarketplaceBackdoorPubKeyHash, eaAppEnvMarketplaceEscrowPubKeyHash, eaAppEnvMarketplaceVersion, eaAppEnvOracleNftMintingPolicyId, eaAppEnvOracleNftTokenName, eaAppEnvOracleOperatorPubKeyHash, eaAppEnvScripts), eaLiftMaybe, eaMarketplaceInfos, eaSubmitTx, initEAApp, runEAApp)
import EA.CLI.Helper (fetchCoreCfg, fetchRootKeyFilePath)
import EA.CLI.Marketplace.Command (MarketplaceCommand (MarketplaceDeployScript, MarketplaceWithdrawScript), MarketplaceDeployCommand (MarketplaceDeployCommand), MarketplaceWithdrawCommand (..))
import EA.Script (oracleValidator)
import EA.Script.Marketplace (MarketplaceInfo (MarketplaceInfo, mktInfoOwner), MarketplaceParams (..))
import EA.Tx.Changeblock.Marketplace (deployScript, withdrawCarbonToken)
import EA.Wallet (eaGetCollateralFromInternalWallet, eaGetInternalAddresses, eaGetaddressFromPaymentKeyHash, eaSelectOref)
import GeniusYield.GYConfig (withCfgProviders)
import GeniusYield.Imports (printf)
import GeniusYield.TxBuilder (runGYTxMonadNode)
import GeniusYield.Types (GYAddress, GYAssetClass (GYToken), addressFromPaymentKeyHash, readSomeSigningKey, signGYTxBody, unsafeAddressFromText, unsafeTokenNameFromHex, validatorHash)
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

-- run withdraw command
runMarketplaceCommand (MarketplaceWithdrawScript MarketplaceWithdrawCommand {..}) = do
  printf "Withdrawing Carbon Token ..."
  coreCfg <- fetchCoreCfg
  providers <- withCfgProviders coreCfg "cli" return
  rootKeyFilePath <- fetchRootKeyFilePath

  env <- initEAApp coreCfg providers rootKeyFilePath 1
  oracleNftPolicyId <- runEAApp env $ asks eaAppEnvOracleNftMintingPolicyId >>= eaLiftMaybe "No Oracle NFT Policy Id"
  oracleNftTknName <- runEAApp env $ asks eaAppEnvOracleNftTokenName >>= eaLiftMaybe "No Oracle NFT Token Name"
  escrowPubkeyHash <- runEAApp env $ asks eaAppEnvMarketplaceEscrowPubKeyHash
  backdoorPubkeyHash <- runEAApp env $ asks eaAppEnvMarketplaceBackdoorPubKeyHash

  version <- runEAApp env $ asks eaAppEnvMarketplaceVersion
  networkId <- runEAApp env $ asks eaAppEnvGYNetworkId
  backdoorSkey <- readSomeSigningKey mktWdrBackdoorKeyPath

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

      backdoorAddr = addressFromPaymentKeyHash networkId backdoorPubkeyHash

  mInfos <- runEAApp env $ eaMarketplaceInfos marketplaceParams
  mInfosWithAddrs <- mapM (withMktInfoOwnerAddrs env) mInfos

  let skeleton = withdrawCarbonToken marketplaceParams (unsafeAddressFromText mktWdrOutAddress) (GYToken mktWdrCarbonPolicyId $ unsafeTokenNameFromHex mktWdrCarbonTokenName) scripts (catMaybes mInfosWithAddrs) mktWdrQty

  print skeleton

  txBody <-
    liftIO $
      runGYTxMonadNode networkId providers [backdoorAddr] backdoorAddr Nothing (return skeleton)

  gyTxId <- runEAApp env $ eaSubmitTx $ signGYTxBody txBody [backdoorSkey]

  printf "\n \n Withdraw Transaction submitted with TxId: %s  \n" gyTxId
  where
    withMktInfoOwnerAddrs :: EAAppEnv -> MarketplaceInfo -> IO (Maybe (GYAddress, MarketplaceInfo))
    withMktInfoOwnerAddrs env minfo@MarketplaceInfo {..} = do
      ownerAddr <- runEAApp env $ eaGetaddressFromPaymentKeyHash mktInfoOwner
      return $ (\(addr, _) -> Just (addr, minfo)) =<< ownerAddr