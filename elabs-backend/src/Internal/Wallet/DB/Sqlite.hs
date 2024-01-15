module Internal.Wallet.DB.Sqlite (
  getAddresses,
  runAutoMigration,
) where

import Data.Aeson qualified as Aeson
import Data.Maybe (fromJust)

import GeniusYield.Types (GYAddress)

import Database.Persist.Sql (
  Entity (entityVal),
  SqlBackend,
  runMigration,
  selectList,
  (==.),
 )

import EA.Api.Types (UserId)

import Internal.Wallet.DB.Schema (
  EntityField (WalletUsed, WalletUser),
  Wallet (..),
  migrateAll,
 )

--------------------------------------------------------------------------------

getAddresses :: (MonadIO m) => UserId -> Bool -> ReaderT SqlBackend m [GYAddress]
getAddresses userId used = do
  addrs <- selectList [WalletUsed ==. used, WalletUser ==. userId] []
  pure $
    map (fromJust . Aeson.decode . fromStrict . walletAddress . entityVal) addrs

runAutoMigration :: (MonadIO m) => ReaderT SqlBackend m ()
runAutoMigration = runMigration migrateAll
