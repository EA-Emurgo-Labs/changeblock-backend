module Internal.Wallet.DB.Sql (
  createWalletIndexPair,
  getWalletIndexPairs,
  getWalletIndexPairs',
  getWalletIndexPairsFromPubkeyhash,
  getInternalWalletIndexPairs,
  getInternalWalletIndexPairs',
  runAutoMigration,
  createAccount,
  listTokens,
  addToken,
  checkToken,
  getUserId,
  saveToUserLookup,
) where

import Data.Tagged (Tagged (Tagged))
import Data.Time (getCurrentTime)
import Database.Persist.Sql (
  Entity (entityKey, entityVal),
  PersistStoreWrite (insert),
  SelectOpt (Desc),
  SqlBackend,
  fromSqlKey,
  runMigration,
  selectKeysList,
  selectList,
  transactionSave,
 )
import EA.Api.Types (UserId)
import Internal.Wallet.DB.Schema (
  Account (Account),
  Address (..),
  Auth (..),
  EntityField (..),
  UserLookup (..),
  Wallet (..),
  migrateAll,
 )

import Database.Esqueleto.Experimental (InnerJoin (InnerJoin), select, val, where_, (&&.), (==.), (?.), (^.))
import Database.Esqueleto.Experimental qualified as E
import Database.Esqueleto.Legacy qualified as EL
import GeniusYield.Types (GYPaymentKeyHash)

--------------------------------------------------------------------------------

-- | Create wallet index pairs
createWalletIndexPair ::
  (MonadIO m) =>
  -- | User ID
  Maybe UserId ->
  -- | Number of pairs
  Int ->
  -- | Collateral
  Bool ->
  ReaderT SqlBackend m ()
createWalletIndexPair userId 1 collateral = do
  -- n need to be 1 because how Nexchange smart contract v1 is implemented
  transactionSave
  time <- liftIO getCurrentTime
  selectKeysList [] [Desc AccountId]
    >>= \case
      [] -> error "No account record found"
      (key : _) ->
        -- replicateM_ n $ do
        do
          addressId <- insert (Address key collateral time)
          void $ insert (Wallet addressId userId time)
  transactionSave
createWalletIndexPair _ _ _ =
  error "Only 1 address can be created at a time"

-- | Get wallet index pairs
getWalletIndexPairs ::
  (MonadIO m) =>
  -- | User ID
  UserId ->
  ReaderT SqlBackend m [(Tagged "Account" Int64, Tagged "Address" Int64)]
getWalletIndexPairs userId = do
  addrs <- EL.select $ EL.from $ \(w `InnerJoin` a) -> do
    EL.on (w ^. WalletAddressId ==. a ^. AddressId)
    where_ (w ^. WalletUser ==. val (Just userId))
    return a

  return $ map getIndexPair addrs

-- | Get wallet index pairs, create if not exist
getWalletIndexPairs' ::
  (MonadIO m) =>
  -- | User ID
  UserId ->
  -- | Number of pairs
  Int ->
  ReaderT SqlBackend m [(Tagged "Account" Int64, Tagged "Address" Int64)]
getWalletIndexPairs' userId n = do
  pairs <- getWalletIndexPairs userId
  when (null pairs) $ createWalletIndexPair (Just userId) n False
  getWalletIndexPairs userId

getWalletIndexPairsFromPubkeyhash :: (MonadIO m) => GYPaymentKeyHash -> ReaderT SqlBackend m (Maybe (Tagged "Account" Int64, Tagged "Address" Int64))
getWalletIndexPairsFromPubkeyhash pkh = do
  address <- EL.select $
    EL.from $ \(ul `InnerJoin` w `InnerJoin` addr) -> do
      EL.on (ul ?. UserLookupUser ==. w ^. WalletUser)
      EL.on (w ^. WalletAddressId ==. addr ^. AddressId)
      where_ (E.not_ (EL.isNothing (w ^. WalletUser)) &&. ul ?. UserLookupPubkeyhash ==. val (Just pkh))
      EL.limit 1
      return addr

  return $ getIndexPair <$> listToMaybe address

-- | Get internal wallet index pairs
getInternalWalletIndexPairs ::
  (MonadIO m) =>
  -- | Collateral
  Bool ->
  ReaderT SqlBackend m [(Tagged "Account" Int64, Tagged "Address" Int64)]
getInternalWalletIndexPairs collateral = do
  addrs <-
    select $
      EL.from $ \(a `InnerJoin` w) -> do
        EL.on (a ^. AddressId ==. w ^. WalletAddressId)
        where_ (EL.isNothing (w ^. WalletUser) &&. a ^. AddressCollateral ==. val collateral)
        return a

  return $ map getIndexPair addrs

-- | Get internal wallet index pairs, create if not exist
getInternalWalletIndexPairs' ::
  (MonadIO m) =>
  -- | Number of pairs
  Int ->
  -- | Collateral
  Bool ->
  ReaderT SqlBackend m [(Tagged "Account" Int64, Tagged "Address" Int64)]
getInternalWalletIndexPairs' n collateral = do
  pairs <- getInternalWalletIndexPairs collateral
  when (null pairs) $ createWalletIndexPair Nothing n collateral
  getInternalWalletIndexPairs collateral

-- | Get index pair from address entity
getIndexPair ::
  -- | Address entity
  Entity Address ->
  (Tagged "Account" Int64, Tagged "Address" Int64)
getIndexPair addr =
  ( Tagged . fromSqlKey . addressAccountId . entityVal $ addr
  , Tagged . fromSqlKey . entityKey $ addr
  )

-- | Get all accounts
getAccounts ::
  (MonadIO m) =>
  ReaderT SqlBackend m [Entity Account]
getAccounts = do
  selectList [] []

-- | Create an account if it does not exist
createAccount :: (MonadIO m) => ReaderT SqlBackend m ()
createAccount = do
  accs <- getAccounts
  when (null accs) $ do
    time <- liftIO getCurrentTime
    void $ insert (Account time)

-- | Run auto migration
runAutoMigration :: (MonadIO m) => ReaderT SqlBackend m ()
runAutoMigration = runMigration migrateAll

-- | Get all tokens
listTokens ::
  (MonadIO m) =>
  ReaderT SqlBackend m [Text]
listTokens = do
  auths :: [Entity Auth] <- selectList [] []
  return $
    authToken . entityVal <$> auths

checkToken :: (MonadIO m) => Text -> ReaderT SqlBackend m Bool
checkToken token = do
  auth <- EL.select $ EL.from $ \a -> do
    where_ (a ^. AuthToken ==. val token)
    EL.limit 1

  return $ not $ null auth

-- | Add a new token
addToken :: (MonadIO m) => Text -> Text -> ReaderT SqlBackend m ()
addToken token notes = do
  time <- liftIO getCurrentTime
  void $ insert (Auth token notes time)

-- | Save derived addresses to user lookup
saveToUserLookup :: (MonadIO m) => UserId -> GYPaymentKeyHash -> ReaderT SqlBackend m ()
saveToUserLookup userId pkh = do
  muserId <- getUserId pkh
  when (Prelude.isNothing muserId) $ do
    time <- liftIO getCurrentTime
    void $ insert (UserLookup userId pkh time)

-- | Get user ID from public key hash
getUserId :: (MonadIO m) => GYPaymentKeyHash -> ReaderT SqlBackend m (Maybe UserId)
getUserId pkh = do
  user <- EL.select $ EL.from $ \ul -> do
    where_ (ul ^. UserLookupPubkeyhash ==. val pkh)
    EL.limit 1
    return ul

  return $ userLookupUser . entityVal <$> listToMaybe user
