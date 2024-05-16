module EA.CLI.Helper (
  fetchCoreCfg,
  fetchSqlPool,
  fetchRootKey,
  fetchRootKeyFilePath,
) where

import Control.Monad.Logger (LoggingT (runLoggingT))
import Data.Maybe (fromJust)
import Data.Pool (Pool)
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (SqlBackend)
import GeniusYield.GYConfig
import Internal.Wallet (RootKey, readRootKey)
import System.Environment (getEnv)

fetchCoreCfg :: IO GYCoreConfig
fetchCoreCfg = do
  coreCfgPath <- cPath
  coreConfigIO coreCfgPath
  where
    cPath :: IO FilePath
    cPath = do
      mPath <- lookupEnv "GY_CORE_CONFIG"
      return $ fromMaybe "config.json" mPath

fetchSqlPool :: IO (Pool SqlBackend)
fetchSqlPool = do
  conStr <- getEnv "DB_CONNECTION"
  runLoggingT (createPostgresqlPool (fromString conStr) 1) showLog
  where
    showLog _ _ _ _ = putStrLn "Database Connected"

fetchRootKey :: IO RootKey
fetchRootKey = do
  rPath <- fetchRootKeyFilePath
  fromJust <$> readRootKey rPath

fetchRootKeyFilePath :: IO FilePath
fetchRootKeyFilePath = do
  mPath <- lookupEnv "ROOT_KEY_PATH"
  return $ fromMaybe "root.key" mPath