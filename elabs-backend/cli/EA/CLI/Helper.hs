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
import System.Directory (doesFileExist)

fetchCoreCfg :: IO GYCoreConfig
fetchCoreCfg = do
  coreCfgPath <- cPath
  isFileValid <- doesFileExist coreCfgPath
  if isFileValid
    then coreConfigIO coreCfgPath
    else cfgPathError
  where
    cPath :: IO FilePath
    cPath = do
      mPath <- lookupEnv "CORE_CONFIG_PATH"
      return $ fromMaybe "config.json" mPath

    cfgPathError :: IO a
    cfgPathError = do
      putStrLn "\n Error: \n Invalid CORE_CONFIG_PATH variable. \n Please set CORE_CONFIG_PATH variable to point to the config file."
      exitFailure

fetchSqlPool :: IO (Pool SqlBackend)
fetchSqlPool = do
  mConStr <- lookupEnv "DB_CONNECTION"
  maybe showError (\conStr -> runLoggingT (createPostgresqlPool (fromString conStr) 1) showLog) mConStr
  where
    showLog _ _ _ _ = putStrLn "Database Connected"
    showError :: IO a
    showError = do
      putStrLn "Error: \n Invalid DB_CONNECTION variable. \n Please set DB_CONNECTION variable to point to the database connection string."
      exitFailure

fetchRootKey :: IO RootKey
fetchRootKey = do
  rPath <- fetchRootKeyFilePath
  fromJust <$> readRootKey rPath

fetchRootKeyFilePath :: IO FilePath
fetchRootKeyFilePath = do
  rPath <- rootKeyPath
  isFileValid <- doesFileExist rPath
  if isFileValid
    then return rPath
    else showError
  where
    rootKeyPath :: IO FilePath
    rootKeyPath = do
      mPath <- lookupEnv "ROOT_KEY_PATH"
      return $ fromMaybe "root.key" mPath

    showError :: IO a
    showError = do
      putStrLn "Error: \n Invalid ROOT_KEY_PATH variable. \n Please set ROOT_KEY_PATH variable to point to the root key file."
      exitFailure