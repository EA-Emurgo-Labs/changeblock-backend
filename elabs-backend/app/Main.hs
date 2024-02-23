module Main (main) where

import Cardano.Mnemonic (MkSomeMnemonic (mkSomeMnemonic))
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (try)
import Control.Monad.Logger (
  LoggingT (runLoggingT),
  fromLogStr,
 )
import Control.Monad.Metrics qualified as Metrics
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Text qualified as T
import Database.Persist.Sqlite (
  createSqlitePool,
  runSqlPool,
 )
import EA (EAAppEnv (..), runEAApp)
import EA.Api (apiServer, apiSwagger, appApi)
import EA.Internal (fromLogLevel)
import EA.Script (Scripts (..))
import EA.Wallet (eaGetInternalAddresses)
import GeniusYield.GYConfig (
  GYCoreConfig (cfgNetworkId),
  coreConfigIO,
  withCfgProviders,
 )
import GeniusYield.Types (GYProviders, gyLog, gyLogInfo)
import Internal.Wallet (
  genRootKeyFromMnemonic,
  readRootKey,
  writeRootKey,
 )
import Internal.Wallet.DB.Sqlite (
  addToken,
  createAccount,
  getTokens,
  runAutoMigration,
 )
import Network.HTTP.Types qualified as HttpTypes
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (
  CorsResourcePolicy (corsRequestHeaders),
  cors,
  simpleCorsResourcePolicy,
 )
import Options.Applicative (
  Parser,
  auto,
  command,
  execParser,
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  option,
  progDesc,
  short,
  showDefault,
  strOption,
  subparser,
  switch,
  value,
 )
import Ply (readTypedScript)
import Relude.Unsafe qualified as Unsafe
import Servant (
  Application,
  Handler (Handler),
  hoistServer,
  serve,
 )
import System.Environment (getEnv)

--------------------------------------------------------------------------------

data Options = Options
  { optionsCoreConfigFile :: !String
  , optionsRootKeyFile :: !String
  , optionsCommand :: !Commands
  }

data AuthTokenOptions = AuthTokenOptions
  { authtokenOptionsToken :: !String
  , authtokenOptionsNotes :: !String
  , authtokenOptionsServerOptions :: !ServerOptions
  }

data InternalAddressesOptions = InternalAddressesOptions
  { internalAddressesCollateral :: !Bool
  , internalAddressesServerOptions :: !ServerOptions
  }

data Commands
  = RunServer ServerOptions
  | ExportSwagger SwaggerOptions
  | GenerateRootKey RootKeyOptions
  | PrintInternalAddresses InternalAddressesOptions
  | PrintAuthTokens ServerOptions
  | AddAuthTokens AuthTokenOptions

data ServerOptions = ServerOptions
  { serverOptionsPort :: !Int
  , serverOptionsSqliteFile :: !String
  , serverOptionsSqlitePoolSize :: !Int
  }
  deriving stock (Show, Read)

data SwaggerOptions = SwaggerOptions
  { swaggerOptionsFile :: !String
  }
  deriving stock (Show, Read)

data RootKeyOptions = RootKeyOptions
  { rootKeyOptionsMnemonic :: !String
  }

options :: Parser Options
options =
  Options
    <$> option
      auto
      ( long "core-config"
          <> help "Core config file path"
          <> showDefault
          <> value "config.json"
      )
    <*> option
      auto
      ( long "root-key"
          <> help "Root key file"
          <> showDefault
          <> value "root.key"
      )
    <*> subparser
      ( command "run" (info (RunServer <$> serverOptions) (progDesc "Run backend server"))
          <> command "swagger" (info (ExportSwagger <$> swaggerOptions) (progDesc "Export swagger api"))
          <> command "genrootkey" (info (GenerateRootKey <$> rootKeyOptions) (progDesc "Root key generation"))
          <> command "internaladdresses" (info (PrintInternalAddresses <$> internalAddressesOptions) (progDesc "Print internal addresses"))
          <> command "tokens" (info (PrintAuthTokens <$> serverOptions) (progDesc "Print available auth tokens"))
          <> command "addtoken" (info (AddAuthTokens <$> authTokenOptions) (progDesc "Add new token"))
      )

internalAddressesOptions :: Parser InternalAddressesOptions
internalAddressesOptions =
  InternalAddressesOptions
    <$> switch (long "collateral" <> help "Collateral")
    <*> serverOptions

authTokenOptions :: Parser AuthTokenOptions
authTokenOptions =
  AuthTokenOptions
    <$> strOption
      ( long "token"
          <> help "Auth token"
      )
    <*> strOption
      ( long "notes"
          <> help "Notes"
      )
    <*> serverOptions

rootKeyOptions :: Parser RootKeyOptions
rootKeyOptions =
  RootKeyOptions
    <$> strOption
      ( long "mnemonic"
          <> help "Mnemonic (15 words)"
      )

serverOptions :: Parser ServerOptions
serverOptions =
  ServerOptions
    <$> option
      auto
      ( long "port"
          <> short 'p'
          <> help "Port"
          <> showDefault
          <> value 8081
      )
    <*> option
      auto
      ( long "sqlite"
          <> help "Sqlite file"
          <> showDefault
          <> value "wallet.db"
      )
    <*> option
      auto
      ( long "sqlite-pool-size"
          <> help "Sqlite pool size"
          <> showDefault
          <> value 10
      )

swaggerOptions :: Parser SwaggerOptions
swaggerOptions =
  SwaggerOptions
    <$> option
      auto
      ( long "outfile"
          <> short 'f'
          <> help "Swagger file output"
          <> showDefault
          <> value "swagger-api.json"
      )

main :: IO ()
main = app =<< execParser opts
  where
    opts =
      info
        (options <**> helper)
        ( fullDesc
            <> progDesc "EMURGUO Africa Backend"
            <> header "elabs-backend - Backend Server"
        )

app :: Options -> IO ()
app opts@(Options {..}) = do
  conf <- coreConfigIO optionsCoreConfigFile

  withCfgProviders conf "app" $
    \providers -> do
      case optionsCommand of
        RunServer srvopts@(ServerOptions {..}) ->
          do
            env <- initEAApp conf providers opts srvopts
            gyLogInfo providers "app" $
              "Starting server at "
                <> "http://localhost:"
                <> show serverOptionsPort
            run serverOptionsPort $ server env
        ExportSwagger (SwaggerOptions {..}) -> do
          let file = swaggerOptionsFile
          gyLogInfo providers "app" $ "Writting swagger file to " <> file
          BL8.writeFile file (encodePretty apiSwagger)
        GenerateRootKey (RootKeyOptions {..}) -> do
          mw <-
            either
              (const (error "Invalid mnemonic"))
              return
              (mkSomeMnemonic @'[15] (words $ T.pack rootKeyOptionsMnemonic))
          writeRootKey optionsRootKeyFile $ genRootKeyFromMnemonic mw
        PrintInternalAddresses (InternalAddressesOptions {..}) -> do
          env <-
            initEAApp conf providers opts internalAddressesServerOptions
          addrs <-
            runEAApp env $ eaGetInternalAddresses internalAddressesCollateral
          putTextLn . show $ fst <$> addrs
        PrintAuthTokens srvOpts -> do
          env <- initEAApp conf providers opts srvOpts
          putTextLn . show $ env.eaAppEnvAuthTokens
        AddAuthTokens (AuthTokenOptions {..}) -> do
          env <- initEAApp conf providers opts authtokenOptionsServerOptions
          pool <- runEAApp env $ asks eaAppEnvSqlPool
          runSqlPool
            (addToken (T.pack authtokenOptionsToken) (T.pack authtokenOptionsNotes))
            pool

initEAApp :: GYCoreConfig -> GYProviders -> Options -> ServerOptions -> IO EAAppEnv
initEAApp conf providers (Options {..}) (ServerOptions {..}) = do
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

  -- Create Sqlite pool and run migrations
  pool <-
    runLoggingT
      ( createSqlitePool
          (T.pack serverOptionsSqliteFile)
          serverOptionsSqlitePoolSize
      )
      $ \_ _ lvl msg ->
        gyLog
          providers
          "db"
          (fromLogLevel lvl)
          (decodeUtf8 $ fromLogStr msg)

  -- migrate tables
  tokens <-
    runSqlPool
      (runAutoMigration >> createAccount >> getTokens)
      pool

  rootKey <- Unsafe.fromJust <$> readRootKey optionsRootKeyFile

  bfIpfsToken <- getEnv "BLOCKFROST_IPFS"

  oracleOutRef <- getEnv "ORACLE_OUTREF"
  oracleScriptHash <- getEnv "ORACLE_SCRIPT_HASH"
  oracleNFTPolicyId <- getEnv "ORACLE_NFT_POLICY_ID"
  oracleNFTTokenName <- getEnv "ORACLE_NFT_TOKENNAME"

  escrowPubkeyHash <- getEnv "ESCROW_PUBKEY_HASH"

  return $
    EAAppEnv
      { eaAppEnvGYProviders = providers
      , eaAppEnvGYNetworkId = cfgNetworkId conf
      , eaAppEnvMetrics = metrics
      , eaAppEnvScripts = scripts
      , eaAppEnvSqlPool = pool
      , eaAppEnvRootKey = rootKey
      , eaAppEnvBlockfrostIpfsProjectId = bfIpfsToken
      , eaAppEnvAuthTokens = tokens
      , eaAppEnvOracleOutRef = fromString oracleOutRef 
      , eaAppEnvOracleScriptHash = fromString oracleScriptHash 
      , eaAppEnvOracleNFTPolicyId = fromString oracleNFTPolicyId 
      , eaAppEnvOracleNFTTokenName= fromString oracleNFTTokenName 
      , eaAppEnvEscrowPubkeyHash= fromString escrowPubkeyHash 
      }

server :: EAAppEnv -> Application
server env =
  cors
    ( const $
        Just
          simpleCorsResourcePolicy
            { corsRequestHeaders = [HttpTypes.hContentType] -- FIXME: better CORS policy
            }
    )
    $ serve appApi
    $ hoistServer appApi (Handler . ExceptT . try . runEAApp env) apiServer
