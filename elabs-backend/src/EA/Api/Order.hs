module EA.Api.Order (
  OrderApi,
  handleOrderApi,
) where

import EA (
  EAApp,
  EAAppEnv (..),
  eaLiftEitherApiError',
  eaLiftEitherServerError,
  eaLiftMaybeApiError,
  eaMarketplaceAtTxOutRef,
  eaMarketplaceInfos,
  eaSubmitTx,
 )
import EA.Api.Order.Types
import EA.Api.Types (
  SubmitTxResponse,
  txBodySubmitTxResponse,
 )
import EA.Script (Scripts, oracleValidator)
import EA.Script.Marketplace (
  MarketplaceInfo (
    MarketplaceInfo,
    mktInfoAmount,
    mktInfoIsSell,
    mktInfoOwner
  ),
  MarketplaceOrderType (..),
  MarketplaceParams (..),
 )
import EA.Script.Marketplace qualified as Marketplace
import EA.Script.Oracle (OracleInfo)
import EA.Tx.Changeblock.Marketplace (
  adjustOrders,
  buy,
  cancel,
  partialBuy,
  sell,
 )
import EA.Wallet (
  eaGetAddressFromPubkeyhash,
  eaGetCollateralFromInternalWallet,
 )
import GeniusYield.TxBuilder (GYTxSkeleton, runGYTxMonadNode)
import GeniusYield.Types
import Internal.Wallet qualified as Wallet
import Servant (
  Description,
  GenericMode ((:-)),
  Get,
  HasServer (ServerT),
  JSON,
  NamedRoutes,
  Post,
  QueryParam,
  ReqBody,
  ToServantApi,
  err400,
  type (:>),
 )
import Servant.Swagger (HasSwagger (toSwagger))

import EA.Api.Order.Exception (OrderApiException (..))
import EA.CommonException (CommonException (..))

import Data.Aeson qualified as Aeson

--------------------------------------------------------------------------------

data OrderApi mode = OrderApi
  { listOrders :: mode :- OrderList
  , orderCreate :: mode :- OrderCreate
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
    { listOrders = handleListOrders
    , orderCreate = handleOrderRequestSell
    , orderBuy = handleOrderBuy
    , orderCancel = handleOrderCancel
    , orderUpdate = handleOrderUpdate
    }

type OrderList =
  "orders"
    :> QueryParam "owner" String
    :> QueryParam "orderType" MarketplaceOrderType
    :> Get '[JSON] [MarketplaceInfo]

type OrderCreate =
  Description "This call allows owner to sell carbon tokens. It creates new sell order for provided amount and price."
    :> "orders"
    :> ReqBody '[JSON] OrderSellRequest
    :> "create_sell_order"
    :> Post '[JSON] SubmitTxResponse

type OrderBuy =
  Description "This api allows buyer to buy carbon token. Buyer will be able to partial buy by providing less amount than what order contains."
    :> "orders"
    :> ReqBody '[JSON] OrderBuyRequest
    :> "buy"
    :> Post '[JSON] SubmitTxResponse

type OrderCancel =
  Description "This Api allows owner to cancel sell order."
    :> "orders"
    :> ReqBody '[JSON] OrderCancelRequest
    :> "cancel"
    :> Post '[JSON] SubmitTxResponse

type OrderUpdate =
  Description "This Api allows owner to update price of sell order."
    :> "orders"
    :> ReqBody '[JSON] OrderUpdateRequest
    :> "update-sale-price"
    :> Post '[JSON] SubmitTxResponse

--------------------------------------------------------------------------------

data MarketplaceApiCtx = MarketplaceApiCtx
  { mktCtxNetworkId :: !GYNetworkId
  , mktCtxProviders :: !GYProviders
  , mktCtxOracleRefInput :: !OracleInfo
  , mktCtxMarketplaceRefScript :: !(Maybe GYTxOutRef)
  , mktCtxScripts :: !Scripts
  , mktCtxCollateral :: (Maybe (GYTxOutRef, Bool), Wallet.PaymentKey)
  , mktCtxParams :: !MarketplaceParams
  }

withMarketplaceApiCtx :: (MarketplaceApiCtx -> EAApp a) -> EAApp a
withMarketplaceApiCtx f = do
  nid <- asks eaAppEnvGYNetworkId
  providers <- asks eaAppEnvGYProviders

  oracleInfo <-
    asks eaAppEnvOracleRefInputUtxo
      >>= eaLiftMaybeApiError OrderNoOracleUtxo

  oracleNftPolicyId <-
    asks eaAppEnvOracleNftMintingPolicyId
      >>= eaLiftMaybeApiError (OrderNoOraclePolicyId oracleInfo)

  oracleNftTknName <-
    asks eaAppEnvOracleNftTokenName
      >>= eaLiftMaybeApiError (OrderNoOracleToken oracleInfo)

  marketplaceRefScript <- asks eaAppEnvMarketplaceRefScriptUtxo
  oracleOperatorPubKeyHash <- asks eaAppEnvOracleOperatorPubKeyHash
  escrowPubkeyHash <- asks eaAppEnvMarketplaceEscrowPubKeyHash
  version <- asks eaAppEnvMarketplaceVersion
  scripts <- asks eaAppEnvScripts
  markertplaceBackdoor <- asks eaAppEnvMarketplaceBackdoorPubKeyHash

  let oracleNftAssetClass = GYToken oracleNftPolicyId oracleNftTknName
      oracleValidatorHash = validatorHash $ oracleValidator oracleNftAssetClass oracleOperatorPubKeyHash scripts
      marketplaceParams =
        MarketplaceParams
          { mktPrmOracleValidator = oracleValidatorHash
          , mktPrmEscrowValidator = escrowPubkeyHash
          , mktPrmVersion = version
          , mktPrmOracleSymbol = oracleNftPolicyId
          , mktPrmOracleTokenName = oracleNftTknName
          , mktPrmBackdoor = markertplaceBackdoor
          }
  -- Get the collateral address and its signing key.
  (collateral, colKey) <-
    eaGetCollateralFromInternalWallet >>= eaLiftMaybeApiError EaNoCollateral

  f $
    MarketplaceApiCtx
      { mktCtxNetworkId = nid
      , mktCtxProviders = providers
      , mktCtxOracleRefInput = oracleInfo
      , mktCtxMarketplaceRefScript = marketplaceRefScript
      , mktCtxScripts = scripts
      , mktCtxCollateral = (collateral, colKey)
      , mktCtxParams = marketplaceParams
      }

handleTx :: MarketplaceApiCtx -> GYAddress -> Wallet.PaymentKey -> GYTxSkeleton 'PlutusV2 -> EAApp SubmitTxResponse
handleTx MarketplaceApiCtx {..} addr addrKey tx = do
  txBody <- liftIO $ runGYTxMonadNode mktCtxNetworkId mktCtxProviders [addr] addr (fst mktCtxCollateral) (return tx)
  void $ eaSubmitTx $ Wallet.signTx txBody [snd mktCtxCollateral, addrKey]
  return $ txBodySubmitTxResponse txBody

handleOrderRequestSell :: OrderSellRequest -> EAApp SubmitTxResponse
handleOrderRequestSell OrderSellRequest {..} = withMarketplaceApiCtx $ \mCtx@MarketplaceApiCtx {..} -> do
  marketplaceInfo <- eaMarketplaceAtTxOutRef sellReqOrderUtxoRef

  -- validate if user can request to sell order
  void $ eaLiftEitherServerError err400 show $ validateRequestSale marketplaceInfo

  -- Owner address and signing Key
  (ownerAddr, ownerKey) <-
    eaLiftMaybeApiError (OrderInvalidOwner marketplaceInfo)
      . find (\(a, _) -> addressToPubKeyHash a == Just (mktInfoOwner marketplaceInfo))
      =<< eaGetAddressFromPubkeyhash owner

  handleTx mCtx ownerAddr ownerKey $
    adjustOrders mktCtxNetworkId marketplaceInfo mktCtxOracleRefInput mktCtxMarketplaceRefScript (toInteger sellReqPrice) (toInteger sellReqAmount) Marketplace.M_SELL mktCtxParams mktCtxScripts
  where
    -- TODO: Better error handling
    validateRequestSale :: MarketplaceInfo -> Either OrderApiException ()
    validateRequestSale mInfo@MarketplaceInfo {..} = do
      when (mktInfoIsSell == M_SELL && sellReqAmount <= fromInteger mktInfoAmount) $
        Left (OrderAlreadyOnSell mInfo)
      when (sellReqAmount > fromInteger mktInfoAmount) $ Left (OrderAmountExceeds mInfo mktInfoAmount)

-- API to handle buying an order
handleOrderBuy :: OrderBuyRequest -> EAApp SubmitTxResponse
handleOrderBuy OrderBuyRequest {..} = withMarketplaceApiCtx $ \mCtx@MarketplaceApiCtx {..} -> do
  marketplaceInfo <- eaMarketplaceAtTxOutRef orderUtxo

  -- validate if buyer can buy order
  void $ eaLiftEitherApiError' id $ validateRequest marketplaceInfo

  -- Get the user address & signing key  from user ID
  (buyerAddr, buyerKey) <-
    eaLiftMaybeApiError (InvalidBuyerPubKey buyer)
      =<< eaGetAddressFromPubkeyhash buyer

  buyerPubkeyHash <- eaLiftMaybeApiError (EaCannotDecodeAddress buyer) (addressToPubKeyHash buyerAddr)
  let tx =
        if isPartial marketplaceInfo
          then -- Partial buy
            partialBuy mktCtxNetworkId marketplaceInfo mktCtxOracleRefInput buyerPubkeyHash (toInteger buyAmount) mktCtxMarketplaceRefScript mktCtxParams mktCtxScripts
          else -- Full buy
            buy mktCtxNetworkId marketplaceInfo mktCtxOracleRefInput buyerPubkeyHash mktCtxMarketplaceRefScript mktCtxParams mktCtxScripts

  handleTx mCtx buyerAddr buyerKey tx
  where
    validateRequest :: MarketplaceInfo -> Either OrderApiException ()
    validateRequest mInfo@MarketplaceInfo {..} = do
      when (mktInfoIsSell /= M_SELL) $ Left $ OrderNotForSale mInfo
      when (buyAmount > fromInteger mktInfoAmount) $ Left $ OrderAmountExceeds mInfo (toInteger buyAmount)

    isPartial :: MarketplaceInfo -> Bool
    isPartial MarketplaceInfo {..} = buyAmount < fromInteger mktInfoAmount

handleOrderCancel :: OrderCancelRequest -> EAApp SubmitTxResponse
handleOrderCancel OrderCancelRequest {..} = withMarketplaceApiCtx $ \mCtx@MarketplaceApiCtx {..} -> do
  marketplaceInfo <- eaMarketplaceAtTxOutRef cancelOrderUtxo

  -- validate if user can cancel order
  void $ eaLiftEitherApiError' id $ validateCancelOrder marketplaceInfo

  -- Owner address and signing Key
  (ownerAddr, ownerKey) <-
    eaLiftMaybeApiError (OrderNoOwnerAddress marketplaceInfo)
      . find (\(a, _) -> addressToPubKeyHash a == Just (mktInfoOwner marketplaceInfo))
      =<< eaGetAddressFromPubkeyhash owner

  handleTx mCtx ownerAddr ownerKey $ cancel mktCtxNetworkId marketplaceInfo mktCtxOracleRefInput mktCtxMarketplaceRefScript mktCtxParams mktCtxScripts
  where
    validateCancelOrder :: MarketplaceInfo -> Either OrderApiException ()
    validateCancelOrder mInfo@MarketplaceInfo {..} = do
      when (mktInfoIsSell /= M_SELL) $ Left (OrderNotForSale mInfo)

handleOrderUpdate :: OrderUpdateRequest -> EAApp SubmitTxResponse
handleOrderUpdate OrderUpdateRequest {..} = withMarketplaceApiCtx $ \mCtx@MarketplaceApiCtx {..} -> do
  marketplaceInfo <- eaMarketplaceAtTxOutRef orderUtxoRef

  -- validate if user can cancel order
  void $ eaLiftEitherApiError' id $ validateUpdateOrder marketplaceInfo

  -- Owner address and signing Key
  (ownerAddr, ownerKey) <-
    eaLiftMaybeApiError (OrderNoOwnerAddress marketplaceInfo)
      . find (\(a, _) -> addressToPubKeyHash a == Just (mktInfoOwner marketplaceInfo))
      =<< eaGetAddressFromPubkeyhash owner

  handleTx mCtx ownerAddr ownerKey $ sell mktCtxNetworkId marketplaceInfo mktCtxOracleRefInput mktCtxMarketplaceRefScript (toInteger updatedPrice) mktCtxParams mktCtxScripts
  where
    validateUpdateOrder :: MarketplaceInfo -> Either OrderApiException ()

    validateUpdateOrder mInfo@MarketplaceInfo {..} = do
      when (updatedPrice <= 0) $ Left $ NegativeOrderPrice mInfo (toInteger updatedPrice)
      when (updatedPrice == fromInteger mktInfoAmount) $ Left $ SameOrderPrice mInfo (toInteger updatedPrice)
      when (mktInfoIsSell /= M_SELL) $ Left $ NonSellOrderPriceUpdate mInfo

handleListOrders :: Maybe String -> Maybe MarketplaceOrderType -> EAApp [MarketplaceInfo]
handleListOrders mOwnerPubkeyHash orderType = withMarketplaceApiCtx $ \MarketplaceApiCtx {..} -> do
  mInfos <- asks eaMarketplaceInfos mktCtxParams
  return $ filter (\m -> filterByOwner m && filterByType m) mInfos
  where
    filterByOwner :: MarketplaceInfo -> Bool
    filterByOwner MarketplaceInfo {..} =
      let parsedPubKeyHash = maybe (Left "") (Aeson.eitherDecode . Aeson.encode) mOwnerPubkeyHash
       in isNothing mOwnerPubkeyHash || parsedPubKeyHash == Right mktInfoOwner

    filterByType :: MarketplaceInfo -> Bool
    filterByType MarketplaceInfo {..} = maybe True (mktInfoIsSell ==) orderType
