module EA (
  EAApp (..),
  EAAppEnv (..),
  runEAApp,
  eaLog,
  eaLogDebug,
  eaLogInfo,
  eaLogWarning,
  eaLogError,
  eaThrow,
  eaCatch,
  eaHandle,
  eaLiftMaybe,
  eaLiftEither,
  eaLiftEither',
  eaSubmitTx,
  eaGetAdaOnlyUTxO,
  eaGetCollateral,
  eaGetCollateral',
  eaGetAddressValue,
  eaGetAddressValue',
  eaMarketplaceAddress,
  eaMarketplaceAtTxOutRef,
  eaMarketplaceInfos,
  eaLiftMaybeServerError,
  eaLiftMaybeApiError,
  eaLiftEitherServerError,
  eaLiftEitherApiError,
  eaLiftEitherApiError',
)
where

import Control.Exception (ErrorCall (ErrorCall), catch, throwIO)
import Control.Monad.Metrics (Metrics, MonadMetrics (getMetrics))
import Data.ByteString.Lazy qualified as LB
import Data.Foldable (minimumBy)
import Data.List qualified as List
import Data.Pool (Pool)
import Database.Persist.Sql (SqlBackend)
import EA.Api.Order.Exception (OrderApiException (OrderNoOraclePolicyId, OrderNoOracleToken, OrderNoOracleUtxo))
import EA.Script (Scripts (..), marketplaceValidator, oracleValidator)
import EA.Script.Marketplace (
  MarketplaceDatum,
  MarketplaceInfo,
  MarketplaceParams (..),
  marketplaceDatumToInfo,
 )
import EA.Script.Oracle (OracleInfo)
import GeniusYield.HTTP.Errors (IsGYApiError)
import GeniusYield.TxBuilder (GYTxMonadException, MonadError (catchError, throwError), adaOnlyUTxOPure, throwAppError, utxoDatumPure)
import GeniusYield.Types
import Internal.AdaPrice (getAdaPrice)
import Internal.Wallet (RootKey)
import Servant (ServerError (errBody), err400)
import UnliftIO (MonadUnliftIO (withRunInIO))

--------------------------------------------------------------------------------

newtype EAApp a = EAApp
  { unEAApp :: ReaderT EAAppEnv IO a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Alternative
    , Monad
    , MonadIO
    , MonadReader EAAppEnv
    , MonadUnliftIO
    )

instance MonadMetrics EAApp where
  getMetrics = asks eaAppEnvMetrics

instance MonadError GYTxMonadException EAApp where
  throwError = eaThrow
  catchError = eaCatch

data EAAppEnv = EAAppEnv
  { eaAppEnvGYProviders :: !GYProviders
  , eaAppEnvGYNetworkId :: !GYNetworkId
  , eaAppEnvMetrics :: !Metrics
  , eaAppEnvScripts :: !Scripts
  , eaAppEnvSqlPool :: !(Pool SqlBackend)
  , eaAppEnvRootKey :: !RootKey
  , eaAppEnvBlockfrostIpfsProjectId :: !String
  , eaAppEnvOracleRefInputUtxo :: !(Maybe OracleInfo)
  , eaAppEnvMarketplaceRefScriptUtxo :: !(Maybe GYTxOutRef)
  , eaAppEnvMarketplaceEscrowPubKeyHash :: !GYPubKeyHash
  , eaAppEnvMarketplaceBackdoorPubKeyHash :: !GYPubKeyHash
  , eaAppEnvMarketplaceVersion :: !GYTokenName
  , eaAppEnvOracleOperatorPubKeyHash :: !GYPubKeyHash
  , eaAppEnvOracleNftMintingPolicyId :: !(Maybe GYMintingPolicyId)
  , eaAppEnvOracleNftTokenName :: !(Maybe GYTokenName)
  }

runEAApp :: EAAppEnv -> EAApp a -> IO a
runEAApp env = flip runReaderT env . unEAApp

--------------------------------------------------------------------------------
-- Logging

eaLog :: (HasCallStack) => GYLogNamespace -> GYLogSeverity -> String -> EAApp ()
eaLog name sev msg = do
  providers <- asks eaAppEnvGYProviders
  liftIO $ gyLog providers name sev msg

eaLogDebug :: (HasCallStack) => GYLogNamespace -> String -> EAApp ()
eaLogDebug name msg = do
  providers <- asks eaAppEnvGYProviders
  liftIO $ gyLogDebug providers name msg

eaLogInfo :: (HasCallStack) => GYLogNamespace -> String -> EAApp ()
eaLogInfo name msg = do
  providers <- asks eaAppEnvGYProviders
  liftIO $ gyLogInfo providers name msg

eaLogWarning :: (HasCallStack) => GYLogNamespace -> String -> EAApp ()
eaLogWarning name msg = do
  providers <- asks eaAppEnvGYProviders
  liftIO $ gyLogWarning providers name msg

eaLogError :: (HasCallStack) => GYLogNamespace -> String -> EAApp ()
eaLogError name msg = do
  providers <- asks eaAppEnvGYProviders
  liftIO $ gyLogError providers name msg

--------------------------------------------------------------------------------
-- Exception

eaLiftMaybe :: String -> Maybe a -> EAApp a
eaLiftMaybe = eaLiftMaybe' . ErrorCall

eaLiftMaybe' :: (Exception e) => e -> Maybe a -> EAApp a
eaLiftMaybe' e Nothing = eaThrow e
eaLiftMaybe' _ (Just x) = return x

eaLiftEither :: (a -> String) -> Either a b -> EAApp b
eaLiftEither f = either (eaThrow . ErrorCall . f) return

eaLiftEither' :: (Exception e) => (a -> e) -> Either a b -> EAApp b
eaLiftEither' f = either (eaThrow . f) return

eaThrow :: (Exception e) => e -> EAApp a
eaThrow e = do
  eaLogError "exception" $ displayException e
  liftIO $ throwIO e

eaCatch :: (Exception e) => EAApp a -> (e -> EAApp a) -> EAApp a
eaCatch action handle = withRunInIO $ \run -> run action `catch` (run . handle)

eaHandle :: (Exception e) => (e -> EAApp a) -> EAApp a -> EAApp a
eaHandle = flip eaCatch

--------------------------------------------------------------------------------
-- Throw server error

eaLiftMaybeServerError :: ServerError -> LB.ByteString -> Maybe a -> EAApp a
eaLiftMaybeServerError error body Nothing = eaThrow $ error {errBody = body}
eaLiftMaybeServerError _ _ (Just a) = pure a

eaLiftMaybeApiError :: (IsGYApiError e, Exception e) => e -> Maybe a -> EAApp a
eaLiftMaybeApiError error Nothing = throwAppError error
eaLiftMaybeApiError _ (Just a) = pure a

eaLiftEitherServerError ::
  ServerError ->
  (a -> LB.ByteString) ->
  Either a b ->
  EAApp b
eaLiftEitherServerError error toBody =
  either (\a -> eaThrow $ error {errBody = toBody a}) pure

eaLiftEitherApiError :: (IsGYApiError e, Exception e) => e -> Either a b -> EAApp b
eaLiftEitherApiError error (Left _) = throwAppError error
eaLiftEitherApiError _ (Right a) = pure a

eaLiftEitherApiError' :: (IsGYApiError e, Exception e) => (a -> e) -> Either a b -> EAApp b
eaLiftEitherApiError' f = either (throwAppError . f) pure

--------------------------------------------------------------------------------
-- Provider functions

eaSubmitTx :: GYTx -> EAApp GYTxId
eaSubmitTx tx = do
  submitTx <- asks (gySubmitTx . eaAppEnvGYProviders)
  eaHandle @SomeException
    eaThrow
    (liftIO $ submitTx tx)

--------------------------------------------------------------------------------
-- Query functions

eaGetAdaOnlyUTxO :: GYAddress -> EAApp [(GYTxOutRef, Natural)]
eaGetAdaOnlyUTxO addr = do
  utxosAtAddress <-
    asks
      (gyQueryUtxosAtAddress' . gyQueryUTxO . eaAppEnvGYProviders)
  utxos <- liftIO $ utxosAtAddress addr Nothing
  return $ adaOnlyUTxOPure utxos

eaGetAddressValue :: [GYAddress] -> EAApp GYValue
eaGetAddressValue addrs = do
  utxosAtAddresses <-
    asks
      (gyQueryUtxosAtAddresses' . gyQueryUTxO . eaAppEnvGYProviders)

  utxos <- liftIO $ utxosAtAddresses addrs
  return $ foldlUTxOs' (\acc utxo -> acc <> utxoValue utxo) (valueFromList []) utxos

eaGetAddressValue' :: [GYAddress] -> ((GYUTxO, Maybe GYDatum) -> GYValue) -> EAApp GYValue
eaGetAddressValue' addrs f = do
  utxosAtAddresses <-
    asks
      (gyQueryUtxosAtAddressesWithDatums . eaAppEnvGYProviders)

  utxos <- liftIO $ utxosAtAddresses addrs
  return $ List.foldl (\acc utxo -> acc <> f utxo) (valueFromList []) utxos

eaGetCollateral ::
  GYAddress ->
  Natural ->
  EAApp (Maybe (GYTxOutRef, Natural))
eaGetCollateral addr minCollateral = do
  xs <- filter (\(_, n) -> n >= minCollateral) <$> eaGetAdaOnlyUTxO addr
  return $ case xs of
    [] -> Nothing
    ys -> Just $ minimumBy (compare `on` snd) ys

eaGetCollateral' :: [GYAddress] -> Natural -> EAApp (Maybe (GYTxOutRef, Natural))
eaGetCollateral' [] _ = return Nothing
eaGetCollateral' (addr : addrs) n = do
  eaGetCollateral addr n >>= \case
    Nothing -> eaGetCollateral' addrs n
    Just oref -> return $ Just oref

eaMarketplaceAtTxOutRef :: GYTxOutRef -> EAApp MarketplaceInfo
eaMarketplaceAtTxOutRef oref = do
  providers <- asks eaAppEnvGYProviders
  utxos <- liftIO $ gyQueryUtxosAtTxOutRefsWithDatums providers [oref]
  utxo <- eaLiftMaybeServerError err400 "No UTXO found" $ listToMaybe utxos
  (addr, val, datum) <-
    eaLiftEither (const "Cannot extract data from UTXO") $
      utxoDatumPure @MarketplaceDatum utxo

  eaLiftEither (const "Cannot create market place info") $
    marketplaceDatumToInfo oref val addr datum Nothing

eaMarketplaceInfos :: MarketplaceParams -> EAApp [MarketplaceInfo]
eaMarketplaceInfos mktPlaceParams = do
  adaPriceResp <- liftIO getAdaPrice
  providers <- asks eaAppEnvGYProviders
  nid <- asks eaAppEnvGYNetworkId
  scripts <- asks eaAppEnvScripts

  let mktPlaceValidator = marketplaceValidator mktPlaceParams scripts
      marketplaceAddr = addressFromValidator nid mktPlaceValidator

  utxos <-
    liftIO $
      gyQueryUtxosAtAddressesWithDatums providers [marketplaceAddr]

  eaLiftEither (const "No marketplace infos found.") $
    sequence $
      filter isRight $
        map (`utxoToMarketplaceInfo` adaPriceResp) utxos
  where
    utxoToMarketplaceInfo :: (GYUTxO, Maybe GYDatum) -> Maybe Double -> Either String MarketplaceInfo
    utxoToMarketplaceInfo t@(utxo, _) adaPrice = do
      (addr, value, datum) <-
        either (Left . show) Right $
          utxoDatumPure @MarketplaceDatum t
      marketplaceDatumToInfo (utxoRef utxo) value addr datum adaPrice

eaMarketplaceAddress :: EAApp GYAddress
eaMarketplaceAddress = do
  nid <- asks eaAppEnvGYNetworkId
  scripts <- asks eaAppEnvScripts

  oracleInfo <-
    asks eaAppEnvOracleRefInputUtxo
      >>= eaLiftMaybeApiError OrderNoOracleUtxo

  oracleNftPolicyId <-
    asks eaAppEnvOracleNftMintingPolicyId
      >>= eaLiftMaybeApiError (OrderNoOraclePolicyId oracleInfo)

  oracleNftTknName <-
    asks eaAppEnvOracleNftTokenName
      >>= eaLiftMaybeApiError (OrderNoOracleToken oracleInfo)

  oracleOperatorPubKeyHash <- asks eaAppEnvOracleOperatorPubKeyHash
  escrowPubkeyHash <- asks eaAppEnvMarketplaceEscrowPubKeyHash
  version <- asks eaAppEnvMarketplaceVersion
  markertplaceBackdoor <- asks eaAppEnvMarketplaceBackdoorPubKeyHash

  let oracleNftAssetClass = GYToken oracleNftPolicyId oracleNftTknName
      oracleValidatorHash = validatorHash $ oracleValidator oracleNftAssetClass oracleOperatorPubKeyHash scripts
      marketplaceParams =
        MarketplaceParams
          { mktPrmOracleValidator = oracleValidatorHash
          , mktPrmEscrowValidator = escrowPubkeyHash
          , mktPrmVersion = version
          , mktPrmOracleSymbol = oracleNftPolicyId
          , mktPrmOracleTokenName = oracleNftTknName
          , mktPrmBackdoor = markertplaceBackdoor
          }
  return $ addressFromValidator nid $ marketplaceValidator marketplaceParams scripts
