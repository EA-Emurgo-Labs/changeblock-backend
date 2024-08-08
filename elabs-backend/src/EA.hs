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
  eaLiftEitherIO,
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
  initEAApp,
)
where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (ErrorCall (ErrorCall), catch)
import Control.Monad.Metrics (Metrics, MonadMetrics (getMetrics))
import Control.Monad.Metrics qualified as Metrics
import Data.ByteString.Lazy qualified as LB
import Data.Foldable (minimumBy)
import Data.List qualified as List
import Data.Pool (Pool)
import Database.Persist.Sql (SqlBackend, runSqlPool)

import Control.Monad.Logger (LoggingT (runLoggingT), fromLogStr)
import Database.Persist.Postgresql (createPostgresqlPool)
import EA.Api.Order.Exception (OrderApiException (OrderNoOraclePolicyId, OrderNoOracleToken, OrderNoOracleUtxo))
import EA.Internal (fromLogLevel)
import EA.Script (Scripts (..), marketplaceValidator, oracleValidator)
import EA.Script.Marketplace (
  MarketplaceDatum,
  MarketplaceInfo,
  MarketplaceParams (..),
  marketplaceDatumToInfo,
 )
import EA.Script.Oracle (OracleInfo, oracleNftAsset, utxoToOracleInfo)
import GeniusYield.GYConfig (GYCoreConfig (cfgNetworkId))
import GeniusYield.HTTP.Errors (IsGYApiError)
import GeniusYield.TxBuilder (GYTxMonadException, MonadError (catchError, throwError), adaOnlyUTxOPure, addressToPubKeyHashIO, throwAppError, utxoDatumPure)
import GeniusYield.Types
import Internal.AdaPrice (getAdaPrice)
import Internal.Constants
import Internal.Wallet (RootKey, readRootKey)
import Internal.Wallet.DB.Sql (createAccount, runAutoMigration)
import Ply (readTypedScript)
import Relude.Unsafe qualified as Unsafe
import Servant (ServerError (errBody), err400)
import System.Directory.Internal.Prelude (getEnv)
import UnliftIO (MonadUnliftIO (withRunInIO), throwIO)

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
  , eaAppEnvMarketplaceEscrowPubKeyHash :: !GYPaymentKeyHash
  , eaAppEnvMarketplaceBackdoorPubKeyHash :: !GYPaymentKeyHash
  , eaAppEnvMarketplaceVersion :: !GYTokenName
  , eaAppEnvOracleOperatorPubKeyHash :: !GYPaymentKeyHash
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

eaLiftEitherIO :: (a -> String) -> Either a b -> IO b
eaLiftEitherIO f = either (fail . f) return

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

-- | Initialize EAApp environment
initEAApp ::
  -- | The Core Config
  GYCoreConfig ->
  -- | The providers
  GYProviders ->
  -- | The root key path
  FilePath ->
  -- | The DB Pool Size
  Int ->
  IO EAAppEnv
initEAApp conf providers rootKeyPath dbPoolSize = do
  -- read .env in the environment
  loadFile defaultConfig

  carbonTokenTypedScript <- readTypedScript "contracts/carbon-token.json"
  carbonNftTypedScript <- readTypedScript "contracts/carbon-nft.json"
  marketplaceTypedScript <- readTypedScript "contracts/marketplace.json"
  oracleTypedScript <- readTypedScript "contracts/oracle.json"
  mintingNftTypedScript <- readTypedScript "contracts/nft.json"

  let scripts =
        Scripts
          { scriptCarbonNftPolicy = carbonNftTypedScript
          , scriptCarbonTokenPolicy = carbonTokenTypedScript
          , scriptMintingNftPolicy = mintingNftTypedScript
          , scriptMarketplaceValidator = marketplaceTypedScript
          , scriptOracleValidator = oracleTypedScript
          }

  metrics <- Metrics.initialize

  -- Create db connection pool and run migrations
  con <- getEnv "DB_CONNECTION"
  pool <-
    runLoggingT
      ( createPostgresqlPool
          (fromString con)
          dbPoolSize
      )
      $ \_ _ lvl msg ->
        gyLog
          providers
          "db"
          (fromLogLevel lvl)
          (decodeUtf8 $ fromLogStr msg)

  -- migrate and Create Account tables
  void $ runSqlPool (runAutoMigration >> createAccount) pool

  rootKey <- Unsafe.fromJust <$> readRootKey rootKeyPath

  bfIpfsToken <- getEnv "BLOCKFROST_IPFS"

  -- Get Oracle Info for reference input
  oracleRefInputUtxo <-
    lookupEnv "ORACLE_UTXO_REF"
      >>= maybe (pure []) (gyQueryUtxosAtTxOutRefsWithDatums providers . List.singleton . fromString)
      >>= maybe (pure Nothing) (return . rightToMaybe . utxoToOracleInfo) . listToMaybe

  (oracleNftPolicyId, oracleNftTokenName) <-
    maybe
      (return (Nothing, Nothing))
      (return . oracleNftPolicyIdAndTokenName . oracleNftAsset)
      oracleRefInputUtxo

  -- Oracle Operator and Escrow PubkeyHash
  escrowPubkeyHash <- addressToPubKeyHashIO $ constEscrowAddress (cfgNetworkId conf)
  operatorPubkeyHash <- addressToPubKeyHashIO $ constOracleOperatorAddress (cfgNetworkId conf)
  backdoorPubkeyHash <- addressToPubKeyHashIO $ constBackdoorAddress $ cfgNetworkId conf

  -- Get Marketplace Utxo for reference script
  marketplaceRefScriptUtxo <-
    lookupEnv "MARKETPLACE_REF_SCRIPT_UTXO"
      >>= maybe (return Nothing) (gyQueryUtxoAtTxOutRef providers . fromString)

  return $
    EAAppEnv
      { eaAppEnvGYProviders = providers
      , eaAppEnvGYNetworkId = cfgNetworkId conf
      , eaAppEnvMetrics = metrics
      , eaAppEnvScripts = scripts
      , eaAppEnvSqlPool = pool
      , eaAppEnvRootKey = rootKey
      , eaAppEnvBlockfrostIpfsProjectId = bfIpfsToken
      , eaAppEnvOracleRefInputUtxo = oracleRefInputUtxo
      , eaAppEnvMarketplaceRefScriptUtxo = utxoRef <$> marketplaceRefScriptUtxo
      , eaAppEnvMarketplaceBackdoorPubKeyHash = paymentKeyHashFromApi $ pubKeyHashToApi backdoorPubkeyHash
      , eaAppEnvOracleOperatorPubKeyHash = paymentKeyHashFromApi $ pubKeyHashToApi operatorPubkeyHash
      , eaAppEnvOracleNftMintingPolicyId = oracleNftPolicyId
      , eaAppEnvOracleNftTokenName = oracleNftTokenName
      , eaAppEnvMarketplaceEscrowPubKeyHash = paymentKeyHashFromApi $ pubKeyHashToApi escrowPubkeyHash
      , eaAppEnvMarketplaceVersion = unsafeTokenNameFromHex "302e302e34" -- v0.0.4
      }
  where
    oracleNftPolicyIdAndTokenName :: Maybe GYAssetClass -> (Maybe GYMintingPolicyId, Maybe GYTokenName)
    oracleNftPolicyIdAndTokenName (Just (GYToken policyId tokename)) = (Just policyId, Just tokename)
    oracleNftPolicyIdAndTokenName _ = (Nothing, Nothing)

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
