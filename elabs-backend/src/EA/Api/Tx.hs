module EA.Api.Tx (
  TxApi,
  handleTxApi,
) where

import Data.Aeson qualified as Aeson
import Data.Swagger qualified as Swagger
import Data.Text qualified as T
import EA (
  EAApp,
  eaAppEnvGYProviders,
  eaCatch,
  eaLiftMaybeServerError,
 )
import GeniusYield.Types (
  GYAwaitTxException (GYAwaitTxException),
  GYAwaitTxParameters (GYAwaitTxParameters),
  GYProviders (gyAwaitTxConfirmed),
  GYTxId,
  txIdFromHex,
 )
import Servant (
  Capture,
  GenericMode ((:-)),
  Get,
  HasServer (ServerT),
  JSON,
  NamedRoutes,
  QueryParam,
  err400,
  type (:>),
 )

--------------------------------------------------------------------------------

data TxApi mode = TxApi
  { txStatus :: mode :- TxStatus
  }
  deriving stock (Generic)

handleTxApi :: ServerT (NamedRoutes TxApi) EAApp
handleTxApi =
  TxApi
    { txStatus = handleTxStatus
    }

type TxStatus =
  "tx"
    :> Capture "id" Text
    :> "status"
    :> QueryParam "confirmation" Word64
    :> Get '[JSON] TxStatusResponse

data TxStatusResponse = TxStatusResponse
  { txStatus :: TxStatusCode
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON, Swagger.ToSchema)

data TxStatusCode = TX_CONFIRMED | TX_PENDING | TX_FAILED
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON, Swagger.ToSchema)

--------------------------------------------------------------------------------

handleTxStatus :: Text -> Maybe Word64 -> EAApp TxStatusResponse
handleTxStatus text confirmation = do
  txid <-
    eaLiftMaybeServerError
      err400
      "Transaction ID not found"
      $ txIdFromHex
        . T.unpack
      $ text
  eaCatch
    (getTxStatus txid (fromMaybe 3 confirmation))
    ( \(GYAwaitTxException _) -> return $ TxStatusResponse TX_PENDING
    )

getTxStatus :: GYTxId -> Word64 -> EAApp TxStatusResponse
getTxStatus txid n = do
  awaitTx <- asks (gyAwaitTxConfirmed . eaAppEnvGYProviders)
  liftIO $
    awaitTx
      (GYAwaitTxParameters 1 0 n)
      txid
  return $ TxStatusResponse TX_CONFIRMED
