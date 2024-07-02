{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Internal.Wallet.DB.Schema (
  EntityField (..),
  Account (..),
  Address (..),
  Wallet (..),
  Auth (..),
  UserLookup (..),
  migrateAll,
) where

import Data.Time (UTCTime)
import Database.Persist (
  EntityField,
  FieldDef (
    fieldAttrs,
    fieldCascade,
    fieldComments,
    fieldDB,
    fieldGenerated,
    fieldHaskell,
    fieldIsImplicitIdColumn,
    fieldReference,
    fieldSqlType,
    fieldStrict,
    fieldType
  ),
 )
import Database.Persist.Sql (
  BackendKey (SqlBackendKey),
 )
import Database.Persist.TH (
  mkMigrate,
  mkPersist,
  persistLowerCase,
  share,
  sqlSettings,
 )
import EA.Api.Types (UserId)
import EA.Orphans (GYPaymentKeyHash)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|

Account
  created UTCTime default=CURRENT_TIMESTAMP
  deriving Show

Address
  accountId AccountId
  collateral Bool default=False
  created UTCTime default=CURRENT_TIMESTAMP
  deriving Show

Wallet
  addressId AddressId
  UniqueAddress addressId
  user UserId Maybe
  created UTCTime default=CURRENT_TIMESTAMP
  deriving Show

UserLookup
  user UserId
  UniqueUser user
  pubkeyhash GYPaymentKeyHash
  UniquePubkeyhash pubkeyhash
  created UTCTime default=CURRENT_TIMESTAMP
  deriving Show

Auth
  token Text
  notes Text
  created UTCTime default=CURRENT_TIMESTAMP
  deriving Show

|]
