module EA.Api.Order (
  OrderApi,
  handleOrderApi,
) where

import Data.Aeson qualified as Aeson
import Data.Swagger qualified as Swagger
import EA (
  EAApp,
  EAAppEnv (..),
  eaLiftMaybe,
  eaMarketplaceAtTxOutRef,
  eaOracleAtTxOutRef,
  eaSubmitTx,
 )
import EA.Api.Types (SubmitTxResponse, UserId, txBodySubmitTxResponse)
import EA.Script.Marketplace (MarketplaceParams (..))
import EA.Tx.Changeblock.Marketplace (buy, cancel)
import EA.Wallet (
  eaGetCollateralFromInternalWallet,
  eaGetInternalAddresses,
  eaSelectOref,
 )
import GeniusYield.TxBuilder (runGYTxMonadNode)
import GeniusYield.Types (
  GYTxOutRef,
  addressToPubKeyHash,
  unsafeTokenNameFromHex,
 )
import Internal.Wallet qualified as Wallet
import Servant (
  Capture,
  GenericMode ((:-)),
  HasServer (ServerT),
  JSON,
  NamedRoutes,
  Post,
  ReqBody,
  ToServantApi,
  type (:>),
 )
import Servant.Swagger (HasSwagger (toSwagger))

--------------------------------------------------------------------------------

data OrderApi mode = OrderApi
  { orderCreate :: mode :- OrderCreate
  , orderBuy :: mode :- OrderBuy
  , orderCancel :: mode :- OrderCancel
  , orderUpdate :: mode :- OrderUpdate
  }
  deriving stock (Generic)

instance HasSwagger (NamedRoutes OrderApi) where
  toSwagger _ = toSwagger (Proxy :: Proxy (ToServantApi OrderApi))

handleOrderApi :: ServerT (NamedRoutes OrderApi) EAApp
handleOrderApi =
  OrderApi
    { orderCreate = handleOrderCreate
    , orderBuy = handleOrderBuy
    , orderCancel = handleOrderCancel
    , orderUpdate = handleOrderUpdate
    }

type OrderCreate =
  "orders"
    :> ReqBody '[JSON] OrderRequest
    :> "create"
    :> Post '[JSON] SubmitTxResponse

type OrderBuy =
  "orders"
    :> ReqBody '[JSON] OrderBuyRequest
    :> "buy"
    :> Post '[JSON] SubmitTxResponse

type OrderCancel =
  "orders"
    :> ReqBody '[JSON] OrderCancelRequest
    :> "cancel"
    :> Post '[JSON] SubmitTxResponse

type OrderUpdate =
  "orders"
    :> ReqBody '[JSON] OrderRequest
    :> Capture "id" Int
    :> "update"
    :> Post '[JSON] SubmitTxResponse

--------------------------------------------------------------------------------

data OrderRequest = OrderRequest
  { userId :: !UserId
  -- ^ The user ID.
  , amount :: !Int
  -- ^ The amount of carbon to mint.
  , sell :: !Int
  -- ^ The sell price per unit of carbon.
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON, Swagger.ToSchema)

data OrderBuyRequest = OrderBuyRequest
  { buyerId :: !UserId
  -- ^ The user ID.
  , amount :: !Int
  -- ^ The amount of carbon to buy.
  , orderId :: !GYTxOutRef
  -- ^ The out ref of order
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON, Swagger.ToSchema)

data OrderCancelRequest = OrderCancelRequest
  { cancelOrderId :: !GYTxOutRef
  -- ^ The out ref of order
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON, Swagger.ToSchema)

--------------------------------------------------------------------------------

-- TODO:

handleOrderCreate :: OrderRequest -> EAApp SubmitTxResponse
handleOrderCreate = error "TODO"

handleOrderBuy :: OrderBuyRequest -> EAApp SubmitTxResponse
handleOrderBuy orderRequest = do
  nid <- asks eaAppEnvGYNetworkId
  providers <- asks eaAppEnvGYProviders
  marketplaceInfo <- eaMarketplaceAtTxOutRef $ orderId orderRequest
  scripts <- asks eaAppEnvScripts

  -- Get oracle info
  oracleOutRef <- asks eaAppEnvOracleOutRef
  oracleInfo <- eaOracleAtTxOutRef oracleOutRef

  -- Get the internal address pairs.
  internalAddrPairs <- eaGetInternalAddresses False

  -- Get oracle validator hash
  oracleScriptHash <- asks eaAppEnvOracleScriptHash

  -- Get the collateral address and its signing key.
  (collateral, colKey) <-
    eaGetCollateralFromInternalWallet >>= eaLiftMaybe "No collateral found"

  (addr, key, _) <-
    eaSelectOref
      internalAddrPairs
      (\r -> collateral /= Just (r, True))
      >>= eaLiftMaybe "No UTxO found"

  buyer <- eaLiftMaybe "Cannot decode address" (addressToPubKeyHash addr)

  -- Get oracle NFT
  oracleNftPolicy <- asks eaAppEnvOracleNFTPolicyId
  oracleNftTokenName <- asks eaAppEnvOracleNFTTokenName
  escrowPubkeyHash <- asks eaAppEnvEscrowPubkeyHash

  -- Get marketplace script ref
  marketplaceScriptOutRef <- asks eaAppEnvMarketplaceScriptOutRef

  let marketParams =
        MarketplaceParams
          { mktPrmOracleValidator = oracleScriptHash
          , mktPrmEscrowValidator = escrowPubkeyHash
          , -- \^ TODO: User proper pubkeyhash of escrow
            mktPrmVersion = unsafeTokenNameFromHex "76312e302e30"
          , -- \^ It can be any string for now using v1.0.0
            mktPrmOracleSymbol = oracleNftPolicy
          , mktPrmOracleTokenName = oracleNftTokenName
          }

  let mMarketplaceRefScript = Just marketplaceScriptOutRef
  let tx =
        buy
          nid
          marketplaceInfo
          oracleInfo
          buyer
          mMarketplaceRefScript
          marketParams
          scripts

  txBody <-
    liftIO $
      runGYTxMonadNode nid providers [addr] addr collateral (return tx)

  void $ eaSubmitTx $ Wallet.signTx txBody [key, colKey]

  return $ txBodySubmitTxResponse txBody

handleOrderCancel :: OrderCancelRequest -> EAApp SubmitTxResponse
handleOrderCancel orderRequest = do
  nid <- asks eaAppEnvGYNetworkId
  providers <- asks eaAppEnvGYProviders
  marketplaceInfo <- eaMarketplaceAtTxOutRef $ cancelOrderId orderRequest
  scripts <- asks eaAppEnvScripts

  -- Get oracle info
  oracleOutRef <- asks eaAppEnvOracleOutRef
  oracleInfo <- eaOracleAtTxOutRef oracleOutRef

  -- Get the internal address pairs.
  internalAddrPairs <- eaGetInternalAddresses False

  -- Get oracle validator hash
  oracleScriptHash <- asks eaAppEnvOracleScriptHash

  -- Get the collateral address and its signing key.
  (collateral, colKey) <-
    eaGetCollateralFromInternalWallet >>= eaLiftMaybe "No collateral found"

  (addr, key, _) <-
    eaSelectOref
      internalAddrPairs
      (\r -> collateral /= Just (r, True))
      >>= eaLiftMaybe "No UTxO found"

  -- Get oracle NFT
  oracleNftPolicy <- asks eaAppEnvOracleNFTPolicyId
  oracleNftTokenName <- asks eaAppEnvOracleNFTTokenName
  escrowPubkeyHash <- asks eaAppEnvEscrowPubkeyHash

  -- Get marketplace script ref
  marketplaceScriptOutRef <- asks eaAppEnvMarketplaceScriptOutRef

  let marketParams =
        MarketplaceParams
          { mktPrmOracleValidator = oracleScriptHash
          , mktPrmEscrowValidator = escrowPubkeyHash
          , -- \^ TODO: User proper pubkeyhash of escrow
            mktPrmVersion = unsafeTokenNameFromHex "76312e302e30"
          , -- \^ It can be any string for now using v1.0.0
            mktPrmOracleSymbol = oracleNftPolicy
          , mktPrmOracleTokenName = oracleNftTokenName
          }

  let mMarketplaceRefScript = Just marketplaceScriptOutRef
  let tx =
        cancel
          nid
          marketplaceInfo
          oracleInfo
          mMarketplaceRefScript
          marketParams
          scripts

  txBody <-
    liftIO $
      runGYTxMonadNode nid providers [addr] addr collateral (return tx)

  void $ eaSubmitTx $ Wallet.signTx txBody [key, colKey]
  return $ txBodySubmitTxResponse txBody

handleOrderUpdate :: OrderRequest -> Int -> EAApp SubmitTxResponse
handleOrderUpdate = error "TODO"
