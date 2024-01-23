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
  getOneShotMintingPolicy,
  eaLiftMaybe,
  eaLiftEither,
  eaLiftEither',
  eaSubmitTx,
  eaGetAdaOnlyUTxO,
  eaGetCollateral,
  eaGetCollateral',
)
where

import Control.Exception (ErrorCall (ErrorCall), catch, throwIO)
import Control.Monad.Metrics (Metrics, MonadMetrics (getMetrics))
import Data.Foldable (minimumBy)
import Data.Pool (Pool)
import Database.Persist.Sql (SqlBackend)
import EA.Script (Scripts (..), oneShotMintingPolicy)
import GeniusYield.GYConfig (GYCoreConfig)
import GeniusYield.TxBuilder (adaOnlyUTxOPure)
import GeniusYield.Types (
  GYAddress,
  GYLogNamespace,
  GYLogSeverity (..),
  GYMintingPolicy,
  GYProviders (..),
  GYQueryUTxO (..),
  GYTx,
  GYTxId,
  GYTxOutRef,
  PlutusVersion (PlutusV2),
  gyLog,
  gyLogDebug,
  gyLogError,
  gyLogInfo,
  gyLogWarning,
 )
import Internal.Wallet (RootKey)
import UnliftIO (MonadUnliftIO (withRunInIO))

--------------------------------------------------------------------------------

newtype EAApp a = EAApp
  { unEAApp :: ReaderT EAAppEnv IO a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader EAAppEnv
    , MonadUnliftIO
    )

instance MonadMetrics EAApp where
  getMetrics = asks eaAppEnvMetrics

data EAAppEnv = EAAppEnv
  { eaAppEnvGYProviders :: !GYProviders
  , eaAppEnvGYCoreConfig :: !GYCoreConfig
  , eaAppEnvMetrics :: !Metrics
  , eaAppEnvScripts :: !Scripts
  , eaAppEnvSqlPool :: !(Pool SqlBackend)
  , eaAppEnvRootKey :: !RootKey
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
-- Reader helpers
getOneShotMintingPolicy :: GYTxOutRef -> EAApp (GYMintingPolicy 'PlutusV2)
getOneShotMintingPolicy oref = asks (oneShotMintingPolicy oref . eaAppEnvScripts)

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
