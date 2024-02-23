module EA.Api.Order (
  OrderApi,
  handleOrderApi,
) where

import Data.Aeson qualified as Aeson
import Data.Swagger qualified as Swagger
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
import EA.Script.Marketplace(MarketplaceParams(..), MarketplaceInfo(..))
import EA (
  EAApp,
  EAAppEnv (eaAppEnvGYNetworkId, eaAppEnvScripts, eaAppEnvGYProviders),
  eaLiftMaybe,
  eaSubmitTx,
  eaMarketplaceAtTxOutRef, 
  eaOracleAtTxOutRef,
 )
import EA.Api.Types (SubmitTxResponse, UserId)
import EA.Script (nftMintingPolicy, oracleValidator)
import EA.Wallet (
  eaGetAddresses,
  eaGetCollateralFromInternalWallet,
  eaGetInternalAddresses,
  eaSelectOref,
 )
import GeniusYield.TxBuilder (runGYTxMonadNode)
import GeniusYield.Types (
  GYAssetClass (GYToken),
  unsafeTokenNameFromHex,
  validatorHash,
  GYTxOutRef, 
  addressToPubKeyHash, 
  mintingPolicyId
 )
import Internal.Wallet qualified as Wallet
import EA.Tx.Changeblock.Marketplace(buy)
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
    :> Capture "id" Int
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

--------------------------------------------------------------------------------

-- TODO:

handleOrderCreate :: OrderRequest -> EAApp SubmitTxResponse
handleOrderCreate = error "TODO"

handleOrderBuy :: OrderBuyRequest -> EAApp SubmitTxResponse
handleOrderBuy orderRequest = do
  nid <- asks eaAppEnvGYNetworkId
  providers <- asks eaAppEnvGYProviders
  _marketplaceInfo <- eaMarketplaceAtTxOutRef $ orderId orderRequest
  _scripts <- asks eaAppEnvScripts
  -- TODO:
  _oracleInfo <- eaOracleAtTxOutRef $ orderId orderRequest

  -- Get the internal address pairs.
  internalAddrPairs <- eaGetInternalAddresses False

  -- Get the user address. We don't need the signing key here.
  (_userAddr, _) <-
    eaLiftMaybe "No addresses found"
      . listToMaybe
      =<< eaGetAddresses (buyerId orderRequest)

  -- Get the collateral address and its signing key.
  (collateral, colKey) <-
    eaGetCollateralFromInternalWallet >>= eaLiftMaybe "No collateral found"

  (addr, key, oref) <-
    eaSelectOref
      internalAddrPairs
      (\r -> collateral /= Just (r, True))
      >>= eaLiftMaybe "No UTxO found"

  _buyer <- eaLiftMaybe "Cannot decode address" (addressToPubKeyHash addr)

  -- TODO: User proper policyId for Oracle NFT
  let oracleNftAsset = mintingPolicyId $ nftMintingPolicy oref _scripts
      oracleNftAssetName = unsafeTokenNameFromHex "43424c"
      orcAssetClass = GYToken oracleNftAsset oracleNftAssetName

      -- TODO: user proper operaor pubkey hash for oracle validator
      orcValidatorHash =
        validatorHash $ oracleValidator orcAssetClass (mktInfoIssuer _marketplaceInfo) _scripts

      _marketParams =
        MarketplaceParams
          { mktPrmOracleValidator = orcValidatorHash
          , mktPrmEscrowValidator = mktInfoIssuer _marketplaceInfo
          , -- \^ TODO: User proper pubkeyhash of escrow
            mktPrmVersion = unsafeTokenNameFromHex "76312e302e30"
          , -- \^ It can be any string for now using v1.0.0
            mktPrmOracleSymbol = oracleNftAsset
          , mktPrmOracleTokenName = oracleNftAssetName
          }
  -- TODO:
  let _mMarketplaceRefScript = Just (orderId orderRequest)
  let tx = buy
            nid
            _marketplaceInfo
            _oracleInfo
            _buyer
            _mMarketplaceRefScript
            _marketParams
            _scripts

  txBody <-
    liftIO $
      runGYTxMonadNode nid providers [addr] addr collateral (return tx)

  void $ eaSubmitTx $ Wallet.signTx txBody [key, colKey]
  error "TODO"

handleOrderCancel :: Int -> EAApp SubmitTxResponse
handleOrderCancel = error "TODO"

handleOrderUpdate :: OrderRequest -> Int -> EAApp SubmitTxResponse
handleOrderUpdate = error "TODO"
