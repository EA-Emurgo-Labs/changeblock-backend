module Setup (
  EACtx (..),
  withEASetup,
  withEaCtx,
  server,
  cleanupSetup,
  createTestCarbonToken,
  sendFundsToAddress,
  checkResponseTxConfirmed,
)
where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (try)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Metrics qualified as Metrics
import Crypto.Hash.SHA256 (hash)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Database.Persist.Postgresql (createPostgresqlPool, rawExecute)
import Database.Persist.Sql (runSqlPool)
import EA (EAAppEnv (..), eaLiftMaybe, runEAApp)
import EA.Api.Types (SubmitTxResponse (SubmitTxResponse, submitTxId), UserId (UserId))
import EA.Routes (appRoutes, routes)
import EA.Script (Scripts (..), marketplaceValidator, nftMintingPolicy, oracleValidator)
import EA.Script.Marketplace (MarketplaceParams (..))
import EA.Script.Oracle (utxoToOracleInfo)
import EA.Test.Helpers (createRootKey)
import EA.Tx.Changeblock.MintIpfsNftCarbonToken (mintIpfsNftCarbonToken)
import EA.Tx.Changeblock.Oracle (createOracle)
import EA.Wallet (eaGetAddresses, eaGetInternalAddresses)
import GeniusYield.Test.Privnet.Ctx (
  Ctx (..),
  User (userAddr),
  ctxProviders,
  ctxRunI,
  submitTx,
 )
import GeniusYield.Test.Privnet.Setup (Setup, makeSetup, withSetup)
import GeniusYield.TxBuilder (addressToPubKeyHashIO, mustHaveOutput, runGYTxQueryMonadNode, scriptAddress)
import GeniusYield.Types
import Internal.Wallet.DB.Sql (
  addToken,
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
import System.Environment (getEnv)

--------------------------------------------------------------------------------

data EACtx = EACtx
  { eaCtxCtx :: Ctx
  , eaCtxEnv :: EAAppEnv
  , eaCtxToken :: !Text
  , eaCtxMarketplaceParams :: !MarketplaceParams
  }

withEASetup :: IO EACtx
withEASetup = do
  withSetup' makeSetup $ \ctx -> do
    -- read .env file
    loadFile defaultConfig

    metrics <- Metrics.initialize
    rootKey <- createRootKey

    carbonNftTypedScript <- readTypedScript "contracts/carbon-nft.json"
    carbonTokenTypedScript <- readTypedScript "contracts/carbon-token.json"
    marketplaceTypedScript <- readTypedScript "contracts/marketplace.json"
    oracleTypedScript <- readTypedScript "contracts/oracle.json"
    mintingNftTypedScript <- readTypedScript "contracts/nft.json"

    let
      scripts =
        Scripts
          { scriptCarbonNftPolicy = carbonNftTypedScript
          , scriptCarbonTokenPolicy = carbonTokenTypedScript
          , scriptMintingNftPolicy = mintingNftTypedScript
          , scriptMarketplaceValidator = marketplaceTypedScript
          , scriptOracleValidator = oracleTypedScript
          }

    -- Create db connection pool and run migrations
    con <- getEnv "DB_CONNECTION_TEST"
    pool <-
      runStderrLoggingT
        ( createPostgresqlPool
            (fromString con)
            20
        )

    bfIpfsToken <- getEnv "BLOCKFROST_IPFS"
    -- TODO: Use valid oracle operator address
    oracleOperatorPubkeyHash <- addressToPubKeyHashIO $ unsafeAddressFromText "addr_test1qpyfg6h3hw8ffqpf36xd73700mkhzk2k7k4aam5jeg9zdmj6k4p34kjxrlgugcktj6hzp3r8es2nv3lv3quyk5nmhtqqexpysh"
    -- TODO: Use valid escrow address
    escrowPubkeyHash <- addressToPubKeyHashIO $ unsafeAddressFromText "addr_test1qpyfg6h3hw8ffqpf36xd73700mkhzk2k7k4aam5jeg9zdmj6k4p34kjxrlgugcktj6hzp3r8es2nv3lv3quyk5nmhtqqexpysh"
    -- TODO: Use valid Backdoor address
    backdoorPubkeyHash <- addressToPubKeyHashIO $ unsafeAddressFromText "addr_test1qpyfg6h3hw8ffqpf36xd73700mkhzk2k7k4aam5jeg9zdmj6k4p34kjxrlgugcktj6hzp3r8es2nv3lv3quyk5nmhtqqexpysh"

    let
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
          , eaAppEnvOracleRefInputUtxo = Nothing
          , eaAppEnvMarketplaceRefScriptUtxo = Nothing
          , eaAppEnvMarketplaceBackdoorPubKeyHash = paymentKeyHashFromApi $ pubKeyHashToApi backdoorPubkeyHash
          , eaAppEnvOracleOperatorPubKeyHash = paymentKeyHashFromApi $ pubKeyHashToApi oracleOperatorPubkeyHash
          , eaAppEnvMarketplaceEscrowPubKeyHash = paymentKeyHashFromApi $ pubKeyHashToApi escrowPubkeyHash
          , eaAppEnvOracleNftMintingPolicyId = Just orcPolicy
          , eaAppEnvOracleNftTokenName = Just orcTn
          , eaAppEnvMarketplaceVersion = unsafeTokenNameFromHex "76312e302e30"
          }

      hashedToken = decodeUtf8 $ hash $ encodeUtf8 $ T.pack "test"

    -- DB migrations
    void $
      runSqlPool
        ( runAutoMigration
            >> createAccount
            >> addToken hashedToken "test Notes"
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
        funder = ctxUserF ctx
        tx =
          mustHaveOutput
            (GYTxOut colAddr (valueFromLovelace 5_000_000) Nothing Nothing)
            <> mustHaveOutput
              (GYTxOut addr (valueFromLovelace 1_000_000_000) Nothing Nothing)

      txBody <- liftIO $ ctxRunI ctx funder $ return tx
      liftIO $ submitTx ctx funder txBody

    gyAwaitTxConfirmed providers (GYAwaitTxParameters 5 5_000_000 1) txId

    putStrLn $ "Send funds to the internal addresses: " <> show txId

    -- Setup Oracle UTXO
    let operatorPubkeyHash = eaAppEnvOracleOperatorPubKeyHash env
        oref = fromString $ show txId ++ "#2"
        scripts = eaAppEnvScripts env
        networkId = eaAppEnvGYNetworkId env
        orcNftPolicy = nftMintingPolicy oref scripts
        oracleNftAsset = mintingPolicyId orcNftPolicy
        orcTokenName = orcTn
        orcAssetClass = GYToken oracleNftAsset orcTokenName
        orcValidator = oracleValidator orcAssetClass operatorPubkeyHash scripts
        orcAddress = addressFromValidator networkId orcValidator
        skeleton = createOracle 1_000_000 oref orcAddress orcTokenName orcNftPolicy
        marketplaceParams =
          MarketplaceParams
            { mktPrmOracleValidator = validatorHash orcValidator
            , mktPrmEscrowValidator = paymentKeyHashFromApi $ pubKeyHashToApi escrowPubkeyHash
            , mktPrmVersion = eaAppEnvMarketplaceVersion env
            , mktPrmOracleSymbol = oracleNftAsset
            , mktPrmOracleTokenName = orcTokenName
            , mktPrmBackdoor = paymentKeyHashFromApi $ pubKeyHashToApi backdoorPubkeyHash
            }

    txBody <-
      liftIO $ ctxRunI ctx (ctxUserF ctx) $ return skeleton

    oracleTxId <- submitTx ctx (ctxUserF ctx) txBody
    gyAwaitTxConfirmed providers (GYAwaitTxParameters 5 5_000_000 1) oracleTxId

    putStrLn $ "Oracle UTXO created: " <> show oracleTxId

    oracleRefInputUtxo <-
      gyQueryUtxosAtTxOutRefsWithDatums providers [txOutRefFromTuple (oracleTxId, 0)]
        >>= maybe (pure Nothing) (return . rightToMaybe . utxoToOracleInfo) . listToMaybe

    let env' =
          env
            { eaAppEnvOracleNftMintingPolicyId = Just oracleNftAsset
            , eaAppEnvOracleNftTokenName = Just orcTokenName
            , eaAppEnvOracleRefInputUtxo = oracleRefInputUtxo
            }

        eaCtx =
          EACtx
            { eaCtxCtx = ctx
            , eaCtxEnv = env'
            , eaCtxToken = "test"
            , eaCtxMarketplaceParams = marketplaceParams
            }

    -- Mint Test Carbon token to use in Order Apis
    carbonUtxoRef <- createTestCarbonToken eaCtx

    putStrLn $ "Carbon Token Minted: " <> show carbonUtxoRef

    return eaCtx
  where
    withSetup' :: IO Setup -> (Ctx -> IO EACtx) -> IO EACtx
    withSetup' ioSetup kont = do
      mvar <- newEmptyMVar
      withSetup ioSetup putStrLn $ \ctx -> do
        res <- kont ctx
        putMVar mvar res

      takeMVar mvar

server :: EAAppEnv -> Application
server env =
  serve appRoutes $
    hoistServer appRoutes (Handler . ExceptT . try . runEAApp env) routes

withEaCtx :: IO EACtx -> (EACtx -> IO ()) -> IO ()
withEaCtx eaCtx f = do
  e' <- eaCtx
  f e'

cleanupSetup :: EACtx -> IO ()
cleanupSetup _ = do
  loadFile defaultConfig

  con <- getEnv "DB_CONNECTION_TEST"

  pool <-
    runStderrLoggingT
      ( createPostgresqlPool
          (fromString con)
          20
      )

  void $
    runSqlPool
      cleanupDatabase
      pool
  where
    cleanupDatabase = do
      rawExecute "DROP TABLE IF EXISTS account CASCADE;" []
      rawExecute "DROP TABLE IF EXISTS wallet CASCADE;" []
      rawExecute "DROP TABLE IF EXISTS address CASCADE;" []
      rawExecute "DROP TABLE IF EXISTS auth CASCADE;" []

createTestCarbonToken :: EACtx -> IO GYTxOutRef
createTestCarbonToken EACtx {..} = do
  let user = ctxUser2 eaCtxCtx
      tokenName = unsafeTokenNameFromHex "74657374"
      nid = eaAppEnvGYNetworkId eaCtxEnv
      providers = eaAppEnvGYProviders eaCtxEnv
      scripts = eaAppEnvScripts eaCtxEnv

  (issuerAddr, _) <- runEAApp eaCtxEnv $ do
    addrs <- eaGetAddresses (UserId 1)
    pure $ head $ NE.fromList addrs

  issuerPubKeyHash <- addressToPubKeyHashIO issuerAddr
  utxos <- gyQueryUtxosAtAddress (eaAppEnvGYProviders eaCtxEnv) (userAddr user) Nothing
  oref <- randomTxOutRef utxos >>= maybe (error "No utxos found") (\(oref, _) -> return oref)
  marketplaceAddr <- runGYTxQueryMonadNode nid providers $ scriptAddress (marketplaceValidator eaCtxMarketplaceParams scripts)

  let tx = mintIpfsNftCarbonToken oref marketplaceAddr issuerAddr (paymentKeyHashFromApi $ pubKeyHashToApi issuerPubKeyHash) (paymentKeyHashFromApi $ pubKeyHashToApi issuerPubKeyHash) tokenName 100 20000 scripts
  txBody <- liftIO $ ctxRunI eaCtxCtx user $ return tx
  txId <- submitTx eaCtxCtx user txBody
  gyAwaitTxConfirmed providers (GYAwaitTxParameters 5 5_000_000 1) txId
  pure $ txOutRefFromTuple (txId, 0)

sendFundsToAddress :: GYAddress -> GYValue -> Ctx -> IO GYTxId
sendFundsToAddress addr value ctx = do
  let
    funder = ctxUserF ctx
    tx =
      mustHaveOutput
        (GYTxOut addr value Nothing Nothing)

  txBody <- liftIO $ ctxRunI ctx funder $ return tx
  txid <- submitTx ctx funder txBody
  gyAwaitTxConfirmed (ctxProviders ctx) (GYAwaitTxParameters 5 5_000_000 1) txid
  pure txid

checkResponseTxConfirmed :: Ctx -> BL.ByteString -> IO GYTxId
checkResponseTxConfirmed ctx resp = do
  SubmitTxResponse {..} <- maybe (fail "Invalid Response") pure $ Aeson.decode @SubmitTxResponse resp
  gyAwaitTxConfirmed (ctxProviders ctx) (GYAwaitTxParameters 5 5_000_000 1) submitTxId
  pure submitTxId