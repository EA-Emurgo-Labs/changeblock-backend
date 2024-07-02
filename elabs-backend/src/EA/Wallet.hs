module EA.Wallet (
  eaGetCollateral,
  eaGetInternalAddresses,
  eaGetInternalAddressesIO,
  eaGetCollateralFromInternalWallet,
  eaGetAddresses,
  eaGetAddresses',
  eaGetaddressFromPaymentKeyHash,
  eaSelectOref,
)
where

import Data.Pool (Pool)
import Database.Persist.Sql (SqlBackend, runSqlPool)
import EA (
  EAApp,
  EAAppEnv (..),
  eaAppEnvSqlPool,
  eaGetCollateral,
  eaLiftEither,
  eaLiftEitherIO,
  eaLiftMaybe,
 )
import EA.Api.Types (UserId)
import GeniusYield.Types (
  GYAddress,
  GYNetworkId,
  GYPaymentKeyHash,
  GYTxOutRef,
  GYUTxO (utxoRef),
  addressToPubKeyHash,
  filterUTxOs,
  gyQueryUtxosAtAddresses,
  paymentKeyHashFromApi,
  pubKeyHashToApi,
  randomTxOutRef,
 )
import Internal.Wallet (PaymentKey, RootKey, deriveAddress)
import Internal.Wallet.DB.Sql (
  getInternalWalletIndexPairs',
  getWalletIndexPairs',
  getWalletIndexPairsFromPubkeyhash,
  saveToUserLookup,
 )

--------------------------------------------------------------------------------

eaGetInternalAddresses :: Bool -> EAApp [(GYAddress, PaymentKey)]
eaGetInternalAddresses collateral = do
  nid <- asks eaAppEnvGYNetworkId
  rootK <- asks eaAppEnvRootKey
  pool <- asks eaAppEnvSqlPool
  liftIO $ eaGetInternalAddressesIO collateral nid rootK pool

eaGetInternalAddressesIO :: Bool -> GYNetworkId -> RootKey -> Pool SqlBackend -> IO [(GYAddress, PaymentKey)]
eaGetInternalAddressesIO collateral nid rootK pool = do
  indexPairs <-
    runSqlPool (getInternalWalletIndexPairs' 1 collateral) pool

  -- \^ Need to be 1 because how ChangeBlock smart contract v1 is implemented
  eaLiftEitherIO id $
    mapM (uncurry $ deriveAddress nid rootK) indexPairs

eaGetAddresses :: UserId -> EAApp [(GYAddress, PaymentKey)]
eaGetAddresses userId = do
  nid <- asks eaAppEnvGYNetworkId
  rootK <- asks eaAppEnvRootKey
  pool <- asks eaAppEnvSqlPool
  eaGetAddresses' userId nid rootK pool

eaGetAddresses' :: UserId -> GYNetworkId -> RootKey -> Pool SqlBackend -> EAApp [(GYAddress, PaymentKey)]
eaGetAddresses' userId nid rootK pool = do
  indexPairs <-
    liftIO $ runSqlPool (getWalletIndexPairs' userId 1) pool
  -- \^ Need to be 1 because how ChangeBlock smart contract v1 is implemented

  -- Save derived pub key hash
  pairs <-
    eaLiftEither id $
      mapM (uncurry $ deriveAddress nid rootK) indexPairs

  pkh <- eaLiftMaybe "cannot get pub key hash from address" $ do
    (addr, _) <- listToMaybe pairs
    addressToPubKeyHash addr
  void . liftIO $ runSqlPool (saveToUserLookup userId (paymentKeyHashFromApi $ pubKeyHashToApi pkh)) pool
  return pairs

eaGetaddressFromPaymentKeyHash :: GYPaymentKeyHash -> EAApp (Maybe (GYAddress, PaymentKey))
eaGetaddressFromPaymentKeyHash pkh = do
  nid <- asks eaAppEnvGYNetworkId
  rootK <- asks eaAppEnvRootKey
  pool <- asks eaAppEnvSqlPool
  indexPairs <-
    liftIO $ runSqlPool (getWalletIndexPairsFromPubkeyhash pkh) pool

  eaLiftEither id $
    mapM (uncurry $ deriveAddress nid rootK) indexPairs

-- FIXME: Maybe Maybe??
eaGetCollateralFromInternalWallet ::
  EAApp (Maybe (Maybe (GYTxOutRef, Bool), PaymentKey))
eaGetCollateralFromInternalWallet = eaGetInternalAddresses True >>= getCollateral

-- FIXME: Maybe Maybe??
getCollateral ::
  [(GYAddress, PaymentKey)] ->
  EAApp (Maybe (Maybe (GYTxOutRef, Bool), PaymentKey))
getCollateral [] = return Nothing
getCollateral ((addr, key) : pairs) = do
  eaGetCollateral addr 5_000_000 >>= \case
    Nothing -> getCollateral pairs
    Just (oref, _) -> return $ Just (Just (oref, True), key)

eaSelectOref ::
  [(GYAddress, PaymentKey)] ->
  (GYTxOutRef -> Bool) ->
  EAApp (Maybe (GYAddress, PaymentKey, GYTxOutRef))
eaSelectOref [] _ = return Nothing
eaSelectOref ((addr, key) : pairs) checkOref = do
  providers <- asks eaAppEnvGYProviders
  utxos <- liftIO $ gyQueryUtxosAtAddresses providers [addr]
  moref <- liftIO $ randomTxOutRef $ filterUTxOs (checkOref . utxoRef) utxos
  case moref of
    Nothing -> eaSelectOref pairs checkOref
    Just (oref, _) -> return $ Just (addr, key, oref)
