module Main (main) where

import Cardano.Mnemonic (MkSomeMnemonic (mkSomeMnemonic))
import Control.Exception (try)
import Crypto.Hash.SHA256 (hash)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.String.Conversions (cs)
import Data.Text qualified as T
import Database.Persist.Sql (runSqlPool)
import EA (EAAppEnv (..), eaLiftMaybe, eaSubmitTx, initEAApp, runEAApp)
import EA.Api (apiSwagger)
import EA.ErrorMiddleware (apiErrorToServerError, exceptionHandler)
import EA.Routes (appRoutes, routes)
import EA.Script (nftMintingPolicy, oracleValidator)
import EA.Script.Marketplace (MarketplaceParams (..))
import EA.Tx.Changeblock.Marketplace (deployScript)
import EA.Tx.Changeblock.Oracle (createOracle)

import EA.Wallet (
  eaGetCollateralFromInternalWallet,
  eaGetInternalAddresses,
  eaSelectOref,
 )
import GeniusYield.GYConfig (
  coreConfigIO,
  withCfgProviders,
 )
import GeniusYield.Imports (printf)
import GeniusYield.TxBuilder (runGYTxMonadNode)
import GeniusYield.Types
import Internal.Wallet (
  genRootKeyFromMnemonic,
  writeRootKey,
 )
import Internal.Wallet qualified as Wallet
import Internal.Wallet.DB.Sql (
  addToken,
  listTokens,
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
import Servant

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
  | CreateOracle CreateOracleOptions
  | DeployScript DeployMarketplaceScriptOptions

data CreateOracleOptions = CreateOracleOptions
  { createOracleServerOptions :: !ServerOptions
  , createOracleOptionsRate :: !Int
  , createOracleOptionsAssetName :: !String
  }
  deriving stock (Show, Read)

data DeployMarketplaceScriptOptions = DeployMarketplaceScriptOptions
  { dplMktplaceServerOptions :: !ServerOptions
  , dplMktplaceAddress :: !Text
  }
  deriving stock (Show, Read)

data ServerOptions = ServerOptions
  { serverOptionsPort :: !Int
  , serverOptionsDBPoolSize :: !Int
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
          <> command "createOracle" (info (CreateOracle <$> createOracleOptions) (progDesc "Create oracle"))
          <> command "deployScript" (info (DeployScript <$> deployScriptOption) (progDesc "Deploy Marketplace Script"))
      )

createOracleOptions :: Parser CreateOracleOptions
createOracleOptions =
  CreateOracleOptions
    <$> serverOptions
    <*> option
      auto
      ( long "rate"
          <> help "Rate"
      )
    <*> strOption
      ( long "asset"
          <> help "Asset name"
          <> showDefault
          <> value "43424c"
      )

deployScriptOption :: Parser DeployMarketplaceScriptOptions
deployScriptOption =
  DeployMarketplaceScriptOptions
    <$> serverOptions
    <*> strOption
      ( long "address"
          <> help "Address"
          <> showDefault
          <> value "addr_test1qpyfg6h3hw8ffqpf36xd73700mkhzk2k7k4aam5jeg9zdmj6k4p34kjxrlgugcktj6hzp3r8es2nv3lv3quyk5nmhtqqexpysh"
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
          <> help "Mnemonic (24 words)"
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
      ( long "db-pool-size"
          <> help "DB connection pool size"
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
app (Options {..}) = do
  conf <- coreConfigIO optionsCoreConfigFile

  withCfgProviders conf "app" $
    \providers -> do
      case optionsCommand of
        RunServer (ServerOptions {..}) ->
          do
            env <- initEAApp conf providers optionsRootKeyFile serverOptionsDBPoolSize
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
              (mkSomeMnemonic @'[24] (words $ T.pack rootKeyOptionsMnemonic))
          writeRootKey optionsRootKeyFile $ genRootKeyFromMnemonic mw
        PrintInternalAddresses (InternalAddressesOptions {..}) -> do
          env <-
            initEAApp conf providers optionsRootKeyFile (serverOptionsDBPoolSize internalAddressesServerOptions)
          addrs <-
            runEAApp env $ eaGetInternalAddresses internalAddressesCollateral
          putTextLn . show $ fst <$> addrs
        PrintAuthTokens srvOpts -> do
          env <- initEAApp conf providers optionsRootKeyFile (serverOptionsDBPoolSize srvOpts)
          pool <- runEAApp env $ asks eaAppEnvSqlPool
          tokens <- runSqlPool listTokens pool
          putTextLn $ show tokens
        AddAuthTokens (AuthTokenOptions {..}) -> do
          env <- initEAApp conf providers optionsRootKeyFile (serverOptionsDBPoolSize authtokenOptionsServerOptions)
          pool <- runEAApp env $ asks eaAppEnvSqlPool
          runSqlPool
            (addToken (decodeUtf8 $ hash $ BS.pack authtokenOptionsToken) (T.pack authtokenOptionsNotes))
            pool
        CreateOracle (CreateOracleOptions {..}) -> do
          env <- initEAApp conf providers optionsRootKeyFile $ serverOptionsDBPoolSize createOracleServerOptions
          internalAddrPairs <- runEAApp env $ eaGetInternalAddresses False

          -- Get the collateral address and its signing key.
          (collateral, colKey) <- runEAApp env $ eaGetCollateralFromInternalWallet >>= eaLiftMaybe "No collateral found"

          (addr, key, oref) <- runEAApp env $ eaSelectOref internalAddrPairs (\r -> collateral /= Just (r, True)) >>= eaLiftMaybe "No UTxO found"

          -- TODO: User proper policyId for Oracle NFT
          let operatorPubkeyHash = eaAppEnvOracleOperatorPubKeyHash env
              scripts = eaAppEnvScripts env
              networkId = eaAppEnvGYNetworkId env
              orcNftPolicy = nftMintingPolicy oref scripts
              oracleNftAsset = mintingPolicyId orcNftPolicy
              orcTokenName = unsafeTokenNameFromHex $ T.pack createOracleOptionsAssetName
              orcAssetClass = GYToken oracleNftAsset orcTokenName
              orcValidator = oracleValidator orcAssetClass operatorPubkeyHash scripts
              orcAddress = addressFromValidator networkId orcValidator
              skeleton = createOracle (fromIntegral createOracleOptionsRate) oref orcAddress orcTokenName orcNftPolicy

          txBody <-
            liftIO $
              runGYTxMonadNode networkId providers [addr] addr collateral (return skeleton)

          gyTxId <- runEAApp env $ eaSubmitTx $ Wallet.signTx txBody [key, colKey]
          printf "\n Oracle created with TxId: %s \n " gyTxId
          printf "\n Operator pubkeyHash: %s \n Operator Address: %s \n" operatorPubkeyHash addr
          printf "\n Oracle NFT Asset: %s \n" orcAssetClass
          printf "\n Oracle Address: %s \n" orcAddress
          printf "\n \n export ORACLE_UTXO_REF=%s#0 \n" gyTxId
        DeployScript (DeployMarketplaceScriptOptions {..}) -> do
          printf "Deploying Marketplace Script to Address: %s" dplMktplaceAddress
          env <- initEAApp conf providers optionsRootKeyFile $ serverOptionsDBPoolSize dplMktplaceServerOptions
          internalAddrPairs <- runEAApp env $ eaGetInternalAddresses False
          oracleNftPolicyId <- runEAApp env $ asks eaAppEnvOracleNftMintingPolicyId >>= eaLiftMaybe "No Oracle NFT Policy Id"
          oracleNftTknName <- runEAApp env $ asks eaAppEnvOracleNftTokenName >>= eaLiftMaybe "No Oracle NFT Token Name"
          escrowPubkeyHash <- runEAApp env $ asks eaAppEnvMarketplaceEscrowPubKeyHash
          backdoorPubkeyHash <- runEAApp env $ asks eaAppEnvMarketplaceBackdoorPubKeyHash

          version <- runEAApp env $ asks eaAppEnvMarketplaceVersion
          networkId <- runEAApp env $ asks eaAppEnvGYNetworkId

          -- Get the collateral address and its signing key.
          (collateral, colKey) <- runEAApp env $ eaGetCollateralFromInternalWallet >>= eaLiftMaybe "No collateral found"

          (addr, key, _oref) <- runEAApp env $ eaSelectOref internalAddrPairs (\r -> collateral /= Just (r, True)) >>= eaLiftMaybe "No UTxO found"

          let scripts = eaAppEnvScripts env
              oracleValidatorHash = validatorHash $ oracleValidator (GYToken oracleNftPolicyId oracleNftTknName) (eaAppEnvOracleOperatorPubKeyHash env) scripts
              marketplaceParams =
                MarketplaceParams
                  { mktPrmOracleValidator = oracleValidatorHash
                  , mktPrmEscrowValidator = escrowPubkeyHash
                  , mktPrmVersion = version
                  , mktPrmOracleSymbol = oracleNftPolicyId
                  , mktPrmOracleTokenName = oracleNftTknName
                  , mktPrmBackdoor = backdoorPubkeyHash
                  }
              skeleton = deployScript (unsafeAddressFromText dplMktplaceAddress) marketplaceParams scripts

          txBody <-
            liftIO $
              runGYTxMonadNode networkId providers [addr] addr collateral (return skeleton)

          gyTxId <- runEAApp env $ eaSubmitTx $ Wallet.signTx txBody [key, colKey]

          printf "\n \n export MARKETPLACE_REF_SCRIPT_UTXO=%s#0 \n" gyTxId

newtype ReadError = ReadError String
  deriving stock (Show)
  deriving anyclass (Exception)

context :: Context '[ErrorFormatter]
context = errorFormatter :. EmptyContext
  where
    errorFormatter :: ErrorFormatter
    errorFormatter _ _ err = err400 {errBody = cs err}

server :: EAAppEnv -> Application
server env =
  cors
    ( const $
        Just
          simpleCorsResourcePolicy
            { corsRequestHeaders = [HttpTypes.hContentType] -- FIXME: better CORS policy
            }
    )
    $ serveWithContext appRoutes context
    $ hoistServer
      appRoutes
      (\ioAct -> Handler . ExceptT $ first (apiErrorToServerError . exceptionHandler) <$> try (runEAApp env ioAct))
    $ routes
