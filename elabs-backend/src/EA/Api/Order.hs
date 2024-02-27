module EA.Api.Order (
  OrderApi,
  handleOrderApi,
) where

import Data.Aeson qualified as Aeson
import Data.Maybe (fromJust)
import Data.Swagger qualified as Swagger
import EA (
  EAApp,
  EAAppEnv (..),
  eaLiftMaybe,
  eaMarketplaceAtTxOutRef,
  eaMarketplaceInfos,
  eaOracleAtTxOutRef,
  eaSubmitTx,
 )
import EA.Api.Types (SubmitTxResponse, UserId, txBodySubmitTxResponse)
import EA.Script (oracleValidator)
import EA.Script.Marketplace (MarketplaceInfo, MarketplaceParams (..), mktInfoAmount)
import EA.Tx.Changeblock.Marketplace (buy, cancel, merge', partialBuy, sell)
import EA.Wallet (
  eaGetAddresses,
  eaGetCollateralFromInternalWallet,
  eaGetInternalAddresses,
 )
import GeniusYield.TxBuilder (runGYTxMonadNode)
import GeniusYield.Types (
  GYAssetClass (GYToken),
  GYTxOutRef,
  addressFromBech32,
  addressToPubKeyHash,
 )
import GeniusYield.Types.Script (validatorHash)
import Internal.Wallet qualified as Wallet
import Servant (
  GenericMode ((:-)),
  Get,
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
  , orderList :: mode :- OrderList
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
    , orderList = handleOrderList
    }

type OrderList =
  "orders"
    :> Get '[JSON] [MarketplaceInfo]

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
    :> ReqBody '[JSON] OrderUpdateRequest
    :> "update"
    :> Post '[JSON] SubmitTxResponse

--------------------------------------------------------------------------------

data OrderRequest = OrderRequest
  { userId :: !UserId
  -- ^ The user ID.
  , amount :: !Int
  -- ^ The amount of carbon to mint.
  , sellPrice :: !Int
  -- ^ The sell price per unit of carbon.
  , orderOriginId :: !GYTxOutRef
  -- ^ The out ref of order
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON, Swagger.ToSchema)

data OrderBuyRequest = OrderBuyRequest
  { buyerId :: !UserId
  -- ^ The user ID.
  , buyAmount :: !Int
  -- ^ The amount of carbon to buy.
  , orderId :: !GYTxOutRef
  -- ^ The out ref of order
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON, Swagger.ToSchema)

data OrderUpdateRequest = OrderUpdateRequest
  { ownerUpdateId :: !UserId
  -- ^ The user ID.
  , updatePrice :: !Int
  , orderUpdateId :: !GYTxOutRef
  -- ^ The out ref of order
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON, Swagger.ToSchema)

data OrderCancelRequest = OrderCancelRequest
  { ownerId :: !UserId
  -- ^ The user ID.
  , cancelOrderId :: !GYTxOutRef
  -- ^ The out ref of order
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON, Swagger.ToSchema)

--------------------------------------------------------------------------------
handleOrderCreate :: OrderRequest -> EAApp SubmitTxResponse
handleOrderCreate orderRequest = do
  nid <- asks eaAppEnvGYNetworkId
  providers <- asks eaAppEnvGYProviders
  marketplaceInfo <- eaMarketplaceAtTxOutRef $ orderOriginId orderRequest
  scripts <- asks eaAppEnvScripts

  -- Get oracle info
  oracleOutRef <- asks eaAppEnvOracleOutRef
  oracleInfo <- eaOracleAtTxOutRef oracleOutRef

  -- Get the user address from user ID. We don't need the signing key here.
  (_, ownerKey) <-
    eaLiftMaybe "No addresses found"
      . listToMaybe
      =<< eaGetAddresses (userId orderRequest)

  -- Get the internal address pairs.
  (internalAddr, internalKey) <-
    eaLiftMaybe "No addresses found"
      . listToMaybe
      =<< eaGetInternalAddresses False

  -- Get the collateral address and its signing key.
  (collateral, colKey) <-
    eaGetCollateralFromInternalWallet >>= eaLiftMaybe "No collateral found"

  -- Get oracle NFT
  oracleNftPolicy <- asks eaAppEnvOracleNFTPolicyId
  oracleNftTokenName <- asks eaAppEnvOracleNFTTokenName
  eaAppEnvOracleOperatorAddr <- asks eaAppEnvOracleOperatorAddr
  escrowPubkeyHash <- asks eaAppEnvEscrowPubkeyHash

  -- Get oracle validator hash
  let orcAssetClass = GYToken oracleNftPolicy oracleNftTokenName
      orcValidatorHash =
        validatorHash $ oracleValidator orcAssetClass (fromJust $ addressToPubKeyHash $ addressFromBech32 eaAppEnvOracleOperatorAddr) scripts

  -- Get marketplace script ref
  marketplaceScriptOutRef <- asks eaAppEnvMarketplaceScriptOutRef
  marketplaceVersion <- asks eaAppEnvMarketplaceVersion

  let marketParams =
        MarketplaceParams
          { mktPrmOracleValidator = orcValidatorHash
          , mktPrmEscrowValidator = escrowPubkeyHash
          , -- \^ TODO: User proper pubkeyhash of escrow
            mktPrmVersion = marketplaceVersion
          , -- \^ It can be any string for now using v1.0.0
            mktPrmOracleSymbol = oracleNftPolicy
          , mktPrmOracleTokenName = oracleNftTokenName
          }

  let mMarketplaceRefScript = Just marketplaceScriptOutRef
  let tx =
        merge'
          nid
          (fromList [marketplaceInfo])
          oracleInfo
          (toInteger $ sellPrice orderRequest)
          (toInteger $ amount orderRequest)
          1
          mMarketplaceRefScript
          marketParams
          scripts

  txBody <-
    liftIO $
      runGYTxMonadNode nid providers [internalAddr] internalAddr collateral (return tx)

  void $ eaSubmitTx $ Wallet.signTx txBody [internalKey, colKey, ownerKey]

  return $ txBodySubmitTxResponse txBody

handleOrderBuy :: OrderBuyRequest -> EAApp SubmitTxResponse
handleOrderBuy orderRequest = do
  nid <- asks eaAppEnvGYNetworkId
  providers <- asks eaAppEnvGYProviders
  marketplaceInfo <- eaMarketplaceAtTxOutRef $ orderId orderRequest
  scripts <- asks eaAppEnvScripts

  -- Get oracle info
  oracleOutRef <- asks eaAppEnvOracleOutRef
  oracleInfo <- eaOracleAtTxOutRef oracleOutRef

  -- Get the user address from user ID. We don't need the signing key here.
  (buyerAddr, buyerKey) <-
    eaLiftMaybe "No addresses found"
      . listToMaybe
      =<< eaGetAddresses (buyerId orderRequest)

  -- Get the internal address pairs.
  (internalAddr, internalKey) <-
    eaLiftMaybe "No addresses found"
      . listToMaybe
      =<< eaGetInternalAddresses False

  -- Get the collateral address and its signing key.
  (collateral, colKey) <-
    eaGetCollateralFromInternalWallet >>= eaLiftMaybe "No collateral found"

  buyer <- eaLiftMaybe "Cannot decode address" (addressToPubKeyHash buyerAddr)

  -- Get oracle NFT
  oracleNftPolicy <- asks eaAppEnvOracleNFTPolicyId
  oracleNftTokenName <- asks eaAppEnvOracleNFTTokenName
  eaAppEnvOracleOperatorAddr <- asks eaAppEnvOracleOperatorAddr
  escrowPubkeyHash <- asks eaAppEnvEscrowPubkeyHash

  -- Get oracle validator hash
  let orcAssetClass = GYToken oracleNftPolicy oracleNftTokenName
      orcValidatorHash =
        validatorHash $ oracleValidator orcAssetClass (fromJust $ addressToPubKeyHash $ addressFromBech32 eaAppEnvOracleOperatorAddr) scripts

  -- Get marketplace script ref
  marketplaceScriptOutRef <- asks eaAppEnvMarketplaceScriptOutRef
  marketplaceVersion <- asks eaAppEnvMarketplaceVersion

  let marketParams =
        MarketplaceParams
          { mktPrmOracleValidator = orcValidatorHash
          , mktPrmEscrowValidator = escrowPubkeyHash
          , -- \^ TODO: User proper pubkeyhash of escrow
            mktPrmVersion = marketplaceVersion
          , -- \^ It can be any string for now using v1.0.0
            mktPrmOracleSymbol = oracleNftPolicy
          , mktPrmOracleTokenName = oracleNftTokenName
          }

  let mMarketplaceRefScript = Just marketplaceScriptOutRef
  let tx =
        if mktInfoAmount marketplaceInfo == toInteger (buyAmount orderRequest)
          then
            buy
              nid
              marketplaceInfo
              oracleInfo
              buyer
              mMarketplaceRefScript
              marketParams
              scripts
          else
            partialBuy
              nid
              marketplaceInfo
              oracleInfo
              buyer
              (toInteger (buyAmount orderRequest))
              mMarketplaceRefScript
              marketParams
              scripts

  txBody <-
    liftIO $
      runGYTxMonadNode nid providers [internalAddr] internalAddr collateral (return tx)

  void $ eaSubmitTx $ Wallet.signTx txBody [internalKey, colKey, buyerKey]

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

  -- Get the user address from user ID. We don't need the signing key here.
  (_, ownerKey) <-
    eaLiftMaybe "No addresses found"
      . listToMaybe
      =<< eaGetAddresses (ownerId orderRequest)

  -- Get the internal address pairs.
  (internalAddr, internalKey) <-
    eaLiftMaybe "No addresses found"
      . listToMaybe
      =<< eaGetInternalAddresses False

  -- Get the collateral address and its signing key.
  (collateral, colKey) <-
    eaGetCollateralFromInternalWallet >>= eaLiftMaybe "No collateral found"

  -- Get oracle NFT
  oracleNftPolicy <- asks eaAppEnvOracleNFTPolicyId
  oracleNftTokenName <- asks eaAppEnvOracleNFTTokenName
  eaAppEnvOracleOperatorAddr <- asks eaAppEnvOracleOperatorAddr
  escrowPubkeyHash <- asks eaAppEnvEscrowPubkeyHash

  -- Get marketplace script ref
  marketplaceScriptOutRef <- asks eaAppEnvMarketplaceScriptOutRef
  marketplaceVersion <- asks eaAppEnvMarketplaceVersion

  -- Get oracle validator hash
  let orcAssetClass = GYToken oracleNftPolicy oracleNftTokenName
      orcValidatorHash =
        validatorHash $ oracleValidator orcAssetClass (fromJust $ addressToPubKeyHash $ addressFromBech32 eaAppEnvOracleOperatorAddr) scripts

  let marketParams =
        MarketplaceParams
          { mktPrmOracleValidator = orcValidatorHash
          , mktPrmEscrowValidator = escrowPubkeyHash
          , -- \^ TODO: User proper pubkeyhash of escrow
            mktPrmVersion = marketplaceVersion
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
      runGYTxMonadNode nid providers [internalAddr] internalAddr collateral (return tx)

  void $ eaSubmitTx $ Wallet.signTx txBody [internalKey, colKey, ownerKey]
  return $ txBodySubmitTxResponse txBody

handleOrderUpdate :: OrderUpdateRequest -> EAApp SubmitTxResponse
handleOrderUpdate orderRequest = do
  nid <- asks eaAppEnvGYNetworkId
  providers <- asks eaAppEnvGYProviders
  marketplaceInfo <- eaMarketplaceAtTxOutRef $ orderUpdateId orderRequest
  scripts <- asks eaAppEnvScripts

  -- Get oracle info
  oracleOutRef <- asks eaAppEnvOracleOutRef
  oracleInfo <- eaOracleAtTxOutRef oracleOutRef

  -- Get the user address from user ID. We don't need the signing key here.
  (_, ownerKey) <-
    eaLiftMaybe "No addresses found"
      . listToMaybe
      =<< eaGetAddresses (ownerUpdateId orderRequest)

  -- Get the internal address pairs.
  (internalAddr, internalKey) <-
    eaLiftMaybe "No addresses found"
      . listToMaybe
      =<< eaGetInternalAddresses False

  -- Get the collateral address and its signing key.
  (collateral, colKey) <-
    eaGetCollateralFromInternalWallet >>= eaLiftMaybe "No collateral found"

  -- Get oracle NFT
  oracleNftPolicy <- asks eaAppEnvOracleNFTPolicyId
  oracleNftTokenName <- asks eaAppEnvOracleNFTTokenName
  eaAppEnvOracleOperatorAddr <- asks eaAppEnvOracleOperatorAddr
  escrowPubkeyHash <- asks eaAppEnvEscrowPubkeyHash

  -- Get marketplace script ref
  marketplaceScriptOutRef <- asks eaAppEnvMarketplaceScriptOutRef
  marketplaceVersion <- asks eaAppEnvMarketplaceVersion

  -- Get oracle validator hash
  let orcAssetClass = GYToken oracleNftPolicy oracleNftTokenName
      orcValidatorHash =
        validatorHash $ oracleValidator orcAssetClass (fromJust $ addressToPubKeyHash $ addressFromBech32 eaAppEnvOracleOperatorAddr) scripts
  let marketParams =
        MarketplaceParams
          { mktPrmOracleValidator = orcValidatorHash
          , mktPrmEscrowValidator = escrowPubkeyHash
          , -- \^ TODO: User proper pubkeyhash of escrow
            mktPrmVersion = marketplaceVersion
          , -- \^ It can be any string for now using v1.0.0
            mktPrmOracleSymbol = oracleNftPolicy
          , mktPrmOracleTokenName = oracleNftTokenName
          }

  let mMarketplaceRefScript = Just marketplaceScriptOutRef
  let tx =
        sell
          nid
          marketplaceInfo
          oracleInfo
          mMarketplaceRefScript
          (toInteger $ updatePrice orderRequest)
          marketParams
          scripts

  txBody <-
    liftIO $
      runGYTxMonadNode nid providers [internalAddr] internalAddr collateral (return tx)

  void $ eaSubmitTx $ Wallet.signTx txBody [internalKey, colKey, ownerKey]

  return $ txBodySubmitTxResponse txBody

handleOrderList :: EAApp [MarketplaceInfo]
handleOrderList = do
  scripts <- asks eaAppEnvScripts
  -- Get oracle NFT
  oracleNftPolicy <- asks eaAppEnvOracleNFTPolicyId
  oracleNftTokenName <- asks eaAppEnvOracleNFTTokenName
  eaAppEnvOracleOperatorAddr <- asks eaAppEnvOracleOperatorAddr
  escrowPubkeyHash <- asks eaAppEnvEscrowPubkeyHash

  -- Get marketplace script ref
  marketplaceVersion <- asks eaAppEnvMarketplaceVersion

  -- Get oracle validator hash
  let orcAssetClass = GYToken oracleNftPolicy oracleNftTokenName
      orcValidatorHash =
        validatorHash $ oracleValidator orcAssetClass (fromJust $ addressToPubKeyHash $ addressFromBech32 eaAppEnvOracleOperatorAddr) scripts

  let marketParams =
        MarketplaceParams
          { mktPrmOracleValidator = orcValidatorHash
          , mktPrmEscrowValidator = escrowPubkeyHash
          , -- \^ TODO: User proper pubkeyhash of escrow
            mktPrmVersion = marketplaceVersion
          , -- \^ It can be any string for now using v1.0.0
            mktPrmOracleSymbol = oracleNftPolicy
          , mktPrmOracleTokenName = oracleNftTokenName
          }

  asks eaMarketplaceInfos marketParams
