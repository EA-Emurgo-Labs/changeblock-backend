{-# LANGUAGE NoOverloadedRecordDot #-}

module EA.Api (
  Api,
  appApi,
  apiSwagger,
  apiServer,
  apiSwaggerUI,
) where

import Control.Lens ((.~), (?~))
import Crypto.Hash.SHA256 (hash)
import Data.Swagger (
  HasBasePath (basePath),
  HasDescription (description),
  HasInfo (info),
  HasTitle (title),
  HasVersion (version),
  Swagger,
 )
import Database.Persist.Sql (runSqlPool)
import EA (EAApp, EAAppEnv (eaAppEnvSqlPool), eaThrow)
import EA.Api.Carbon (CarbonApi, handleCarbonApi)
import EA.Api.Order (OrderApi, handleOrderApi)
import EA.Api.Tx (TxApi, handleTxApi)
import EA.Api.Types (AuthorizationHeader (unAuthorizationHeader))
import EA.Api.Wallet (WalletApi, handleWalletApi)
import GeniusYield.HTTP.Errors (GYApiError (GYApiError, gaeErrorCode, gaeHttpStatus, gaeMsg), IsGYApiError (toApiError))
import GeniusYield.TxBuilder (throwAppError)
import Internal.Wallet.DB.Sql (checkToken)
import Network.HTTP.Types (status401)
import Servant (
  GenericMode ((:-)),
  HasServer (ServerT),
  Header,
  NamedRoutes,
  Raw,
  err401,
  hoistServer,
  serveDirectoryFileServer,
  (:>),
 )
import Servant.Swagger (toSwagger)

--------------------------------------------------------------------------------

type Api =
  "v0"
    :> Header "Authorization" AuthorizationHeader
    :> NamedRoutes NexchangeApi

data NexchangeApi mode = NexchangeApi
  { txApi :: mode :- NamedRoutes TxApi
  , walletApi :: mode :- NamedRoutes WalletApi
  , carbonApi :: mode :- CarbonApi
  , orderApi :: mode :- NamedRoutes OrderApi
  }
  deriving stock (Generic)

apiSwagger :: Swagger
apiSwagger =
  toSwagger appApi
    & info
    . title
    .~ "Nexchange API"
      & info
      . version
    .~ "1.0"
      & info
      . description
    ?~ "The Nexchange API."
      & (basePath ?~ "/api")

appApi :: Proxy Api
appApi = Proxy

apiServer :: ServerT Api EAApp
apiServer = nexchangeServer'

apiSwaggerUI :: ServerT Raw EAApp
apiSwaggerUI = serveDirectoryFileServer "swagger-ui"

nexchangeServer ::
  Maybe AuthorizationHeader ->
  ServerT (NamedRoutes NexchangeApi) EAApp
nexchangeServer _ =
  NexchangeApi
    { txApi = handleTxApi
    , walletApi = handleWalletApi
    , carbonApi = handleCarbonApi
    , orderApi = handleOrderApi
    }

data UnAuthorizedError = UnAuthorizedError
  deriving stock (Show)
  deriving anyclass (Exception)

instance IsGYApiError UnAuthorizedError where
  toApiError _ =
    GYApiError
      { gaeErrorCode = "UNAUTHORIZED"
      , gaeHttpStatus = status401
      , gaeMsg = "Unauthorized access to the API. Please provide a valid token in Header."
      }

nexchangeServer' ::
  Maybe AuthorizationHeader ->
  ServerT (NamedRoutes NexchangeApi) EAApp
nexchangeServer' maybeAuthHeader =
  hoistServer
    (Proxy @(NamedRoutes NexchangeApi))
    run
    (nexchangeServer maybeAuthHeader)
  where
    run :: EAApp a -> EAApp a
    run action = case maybeAuthHeader of
      Nothing -> eaThrow err401
      Just token -> do
        pool <- asks eaAppEnvSqlPool
        let hashedToken = decodeUtf8 $ hash $ encodeUtf8 $ unAuthorizationHeader token

        isValidToken <- liftIO $ runSqlPool (checkToken hashedToken) pool
        unless
          isValidToken
          (throwAppError UnAuthorizedError)
        action
