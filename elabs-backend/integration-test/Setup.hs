module Setup (
  EACtx (..),
  withEASetup,
  server,
  cleanupSetup,
)
where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (try)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Metrics qualified as Metrics
import Data.List ((!!))
import Data.Text qualified as T
import Database.Persist.Sqlite (createSqlitePool, rawExecute, runSqlPool)
import EA (EAAppEnv (..), eaLiftMaybe, runEAApp)
import EA.Routes (appRoutes, routes)
import EA.Script (Scripts (..))
import EA.Test.Helpers (createRootKey)
import EA.Wallet (eaGetInternalAddresses)
import GeniusYield.Test.Privnet.Ctx (
  Ctx (..),
  User,
  ctxProviders,
  ctxRunI,
  submitTx,
 )
import GeniusYield.Test.Privnet.Setup (Setup, withSetup)
import GeniusYield.TxBuilder (addressToPubKeyHashIO, mustHaveOutput)
import GeniusYield.Types
import Internal.Wallet.DB.Sqlite (
  createAccount,
  runAutoMigration,
 )
import Ply (readTypedScript)
import Servant (
  Application,
  Handler (Handler),
  hoistServer,
  serve,
 )
import System.Directory (doesFileExist, removeFile)
import System.Environment (getEnv)
import System.FilePath.Glob (glob)
import System.Random (randomRIO)

--------------------------------------------------------------------------------

data EACtx = EACtx
  { eaCtxCtx :: Ctx
  , eaCtxEnv :: EAAppEnv
  }

withEASetup ::
  (Ctx -> User) ->
  IO Setup ->
  (String -> IO ()) ->
  (EACtx -> IO ()) ->
  IO ()
withEASetup getUser ioSetup putLog kont =
  withSetup ioSetup putLog $ \ctx -> do
    -- read .env file
    loadFile defaultConfig

    id <- randomString 10

    metrics <- Metrics.initialize
    rootKey <- createRootKey

    carbonNftTypedScript <- readTypedScript "contracts/carbon-nft.json"
    carbonTokenTypedScript <- readTypedScript "contracts/carbon-token.json"
    marketplaceTypedScript <- readTypedScript "contracts/marketplace.json"
    oracleTypedScript <- readTypedScript "contracts/oracle.json"
    mintingNftTypedScript <- readTypedScript "contracts/nft.json"

    let
      optionsSqliteFile = "wallet.test." <> id <> ".db"

      scripts =
        Scripts
          { scriptCarbonNftPolicy = carbonNftTypedScript
          , scriptCarbonTokenPolicy = carbonTokenTypedScript
          , scriptMintingNftPolicy = mintingNftTypedScript
          , scriptMarketplaceValidator = marketplaceTypedScript
          , scriptOracleValidator = oracleTypedScript
          }

    -- Create Sqlite pool and run migrations
    pool <-
      runStderrLoggingT
        ( createSqlitePool
            (T.pack optionsSqliteFile)
            20
        )

    bfIpfsToken <- getEnv "BLOCKFROST_IPFS"
    -- TODO: Use valid oracle operator address
    oracleOperatorPubkeyHash <- addressToPubKeyHashIO $ unsafeAddressFromText "addr_test1qpyfg6h3hw8ffqpf36xd73700mkhzk2k7k4aam5jeg9zdmj6k4p34kjxrlgugcktj6hzp3r8es2nv3lv3quyk5nmhtqqexpysh"
    -- TODO: Use valid escrow address
    escrowPubkeyHash <- addressToPubKeyHashIO $ unsafeAddressFromText "addr_test1qpyfg6h3hw8ffqpf36xd73700mkhzk2k7k4aam5jeg9zdmj6k4p34kjxrlgugcktj6hzp3r8es2nv3lv3quyk5nmhtqqexpysh"

    let
      tokens = ["AAAA"]
      providers = ctxProviders ctx
      (orcPolicy, orcTn) = (fromString "492335da5d8eb86f076717211c3e7e4711eedf8c358923e925b3c3b5", unsafeTokenNameFromHex "6f72636c65")
      env =
        EAAppEnv
          { eaAppEnvGYProviders = providers
          , eaAppEnvGYNetworkId = GYPrivnet
          , eaAppEnvMetrics = metrics
          , eaAppEnvScripts = scripts
          , eaAppEnvSqlPool = pool
          , eaAppEnvRootKey = rootKey
          , eaAppEnvBlockfrostIpfsProjectId = bfIpfsToken
          , eaAppEnvAuthTokens = tokens
          , eaAppEnvOracleRefInputUtxo = Nothing
          , eaAppEnvMarketplaceRefScriptUtxo = Nothing
          , eaAppEnvOracleOperatorPubKeyHash = oracleOperatorPubkeyHash
          , eaAppEnvMarketplaceEscrowPubKeyHash = escrowPubkeyHash
          , eaAppEnvOracleNftMintingPolicyId = Just orcPolicy
          , eaAppEnvOracleNftTokenName = Just orcTn
          , eaAppEnvMarketplaceVersion = unsafeTokenNameFromHex "76312e302e30"
          }

    -- DB migrations
    void $
      runSqlPool
        ( rawExecute "PRAGMA busy_timeout=100000" []
            >> runAutoMigration
            >> createAccount
        )
        pool

    -- Adding funds to the internal collateral address
    txId <- runEAApp env $ do
      (addr, _) <-
        eaLiftMaybe "No internal address found"
          . viaNonEmpty head
          =<< eaGetInternalAddresses False
      (colAddr, _) <-
        eaLiftMaybe "No internal collateral address found"
          . viaNonEmpty head
          =<< eaGetInternalAddresses True

      let
        user = getUser ctx
        tx =
          mustHaveOutput
            (GYTxOut colAddr (valueFromLovelace 5_000_000) Nothing Nothing)
            <> mustHaveOutput
              (GYTxOut addr (valueFromLovelace 1_000_000_000) Nothing Nothing)

      txBody <- liftIO $ ctxRunI ctx user $ return tx
      liftIO $ submitTx ctx user txBody

    gyAwaitTxConfirmed providers (GYAwaitTxParameters 5 5_000_000 1) txId

    putLog $ "Send funds to the internal addresses: " <> show txId

    kont $ EACtx ctx env

server :: EAAppEnv -> Application
server env =
  serve appRoutes $
    hoistServer appRoutes (Handler . ExceptT . try . runEAApp env) routes

randomString :: Int -> IO String
randomString len = replicateM len randomChar
  where
    randomChar :: IO Char
    randomChar = do
      let chars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']
      randomRIO (0, length chars - 1) <&> (chars !!)

cleanupSetup :: Setup -> IO ()
cleanupSetup _ = do
  files <- glob "wallet.test.*.db*" -- FIXME: check optionsSqliteFile
  mapM_ (\file -> doesFileExist file >>= flip when (removeFile file)) files
