module EA.Api.Carbon (
  CarbonApi,
  handleCarbonApi,
)
where

import Data.Aeson qualified as Aeson
import Data.Swagger qualified as Swagger
import Data.Text qualified as T
import EA (
  EAApp,
  EAAppEnv (
    eaAppEnvGYNetworkId,
    eaAppEnvGYProviders,
    eaAppEnvMarketplaceBackdoorPubKeyHash,
    eaAppEnvMarketplaceEscrowPubKeyHash,
    eaAppEnvMarketplaceVersion,
    eaAppEnvOracleNftMintingPolicyId,
    eaAppEnvOracleNftTokenName,
    eaAppEnvOracleOperatorPubKeyHash,
    eaAppEnvOracleRefInputUtxo,
    eaAppEnvScripts
  ),
  eaLiftEitherServerError,
  eaLiftMaybeApiError,
  eaSubmitTx,
 )
import EA.Api.Ctx (runSkeletonF)
import EA.Api.Order.Exception (OrderApiException (OrderNoOraclePolicyId, OrderNoOracleToken, OrderNoOracleUtxo))
import EA.Api.Types (
  CarbonMintRequest (amount, sell, userId),
  SubmitTxResponse,
  txBodySubmitTxResponse,
 )
import EA.CommonException (CommonException (EaCustomError, EaInvalidAddres, EaInvalidUserAddress, EaNoCollateral, EaNoUtxoForAddress))
import EA.Orphans (MultipartFormDataTmp)
import EA.Script (carbonNftMintingPolicy, marketplaceValidator, oracleValidator)
import EA.Script.Marketplace (MarketplaceParams (..))
import EA.Tx.Changeblock.MintIpfsNftCarbonToken (mintIpfsNftCarbonToken)
import EA.Wallet (
  eaGetAddresses,
  eaGetCollateralFromInternalWallet,
  eaSelectOref,
 )
import GeniusYield.TxBuilder (
  runGYTxQueryMonadIO,
  scriptAddress,
 )
import GeniusYield.Types (
  GYAssetClass (GYToken),
  mintingPolicyId,
  paymentKeyHashFromApi,
  pubKeyHashToApi,
  unsafeTokenNameFromHex,
  validatorHash,
 )
import GeniusYield.Types.Address (addressToPubKeyHash)
import Internal.Ipfs (ipfsAddFile, ipfsPinObject)
import Internal.Ipfs.Types (IpfsAddResponse (..), IpfsPin (..))
import Internal.Wallet qualified as Wallet
import Servant (JSON, Post, Tagged, err400, type (:>))
import Servant.Multipart (
  MultipartData,
  Tmp,
  lookupFile,
  lookupInput,
 )
import Servant.Swagger (HasSwagger (toSwagger))

--------------------------------------------------------------------------------

type CarbonApi = CarbonMint

type CarbonMint =
  "carbon"
    :> MultipartFormDataTmp
    :> "mint"
    :> Post '[JSON] CarbonMintResponse

--------------------------------------------------------------------------------
-- Swagger woraround instance for 'CarbonApi'

instance {-# OVERLAPPING #-} HasSwagger CarbonApi where
  toSwagger _ = toSwagger (Proxy :: Proxy CarbonMint')

type CarbonMint' =
  "carbon"
    :> Tagged CarbonMintRequest MultipartFormDataTmp
    :> "mint"
    :> Post '[JSON] CarbonMintResponse

--------------------------------------------------------------------------------

data CarbonMintResponse = CarbonMintResponse
  { ipfsHash :: !Text
  , ipfsName :: !Text
  , ipfsSize :: !Text
  , ipfsPinningState :: !Text
  , carbonAsset :: !GYAssetClass
  , submitTxInfo :: !SubmitTxResponse
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.ToJSON, Swagger.ToSchema)

--------------------------------------------------------------------------------

handleCarbonApi ::
  MultipartData Tmp ->
  EAApp CarbonMintResponse
handleCarbonApi multipartData = do
  filePart <-
    eaLiftEitherServerError err400 fromString $
      lookupFile "file" multipartData
  dataPart <-
    eaLiftEitherServerError err400 fromString $
      lookupInput "data" multipartData

  request <-
    eaLiftMaybeApiError (EaCustomError "INVALID_JSON_REQUEST" "Cannot Decode JSON data") $
      Aeson.decode @CarbonMintRequest $
        encodeUtf8 dataPart

  nid <- asks eaAppEnvGYNetworkId
  providers <- asks eaAppEnvGYProviders
  scripts <- asks eaAppEnvScripts
  marketplaceVersion <- asks eaAppEnvMarketplaceVersion
  escrowPubkeyHash <- asks eaAppEnvMarketplaceEscrowPubKeyHash
  backdoorPubkeyHash <- asks eaAppEnvMarketplaceBackdoorPubKeyHash
  operatorPubkeyHash <- asks eaAppEnvOracleOperatorPubKeyHash
  oracleInfo <-
    asks eaAppEnvOracleRefInputUtxo
      >>= eaLiftMaybeApiError OrderNoOracleUtxo
  oracleAssetName <- asks eaAppEnvOracleNftTokenName >>= eaLiftMaybeApiError (OrderNoOracleToken oracleInfo)
  oraclePolicyId <- asks eaAppEnvOracleNftMintingPolicyId >>= eaLiftMaybeApiError (OrderNoOraclePolicyId oracleInfo)

  -- Get the user address. We don't need the signing key here.
  (userAddr, userPKey) <-
    eaLiftMaybeApiError (EaInvalidUserAddress $ userId request)
      . listToMaybe
      =<< eaGetAddresses (userId request)

  (userAddr, userKey, userOref) <-
    eaSelectOref
      [(userAddr, userPKey)]
      (const True)
      >>= eaLiftMaybeApiError (EaNoUtxoForAddress userAddr)

  -- Get the collateral address and its signing key.
  (collateral, colKey) <-
    eaGetCollateralFromInternalWallet >>= eaLiftMaybeApiError EaNoCollateral

  -- TODO: Issuer should be Internal Wallet address ?
  issuer <- eaLiftMaybeApiError (EaInvalidAddres userAddr) (addressToPubKeyHash userAddr)
  owner <- eaLiftMaybeApiError (EaInvalidAddres userAddr) (addressToPubKeyHash userAddr)

  let orcValidatorHash =
        validatorHash $ oracleValidator (GYToken oraclePolicyId oracleAssetName) operatorPubkeyHash scripts

      marketParams =
        MarketplaceParams
          { mktPrmOracleValidator = orcValidatorHash
          , mktPrmEscrowValidator = escrowPubkeyHash
          , mktPrmVersion = marketplaceVersion
          , mktPrmOracleSymbol = oraclePolicyId
          , mktPrmOracleTokenName = oracleAssetName
          , mktPrmBackdoor = backdoorPubkeyHash
          }

  marketplaceAddress <-
    liftIO $
      runGYTxQueryMonadIO nid providers $
        scriptAddress (marketplaceValidator marketParams scripts)

  ipfsAddResp <- ipfsAddFile filePart
  ipfsPinObjResp <- ipfsPinObject ipfsAddResp.ipfs_hash

  let
    tokenName =
      unsafeTokenNameFromHex $
        T.take 10 $
          T.append "CBLK" ipfsAddResp.ipfs_hash

    tx =
      mintIpfsNftCarbonToken
        userOref
        marketplaceAddress
        userAddr
        (paymentKeyHashFromApi $ pubKeyHashToApi owner)
        (paymentKeyHashFromApi $ pubKeyHashToApi issuer)
        tokenName
        (toInteger $ sell request)
        (toInteger $ amount request)
        scripts

    carbonNftAsset = GYToken (mintingPolicyId $ carbonNftMintingPolicy userOref tokenName scripts) tokenName

  txBody <- liftIO $ runSkeletonF nid providers [userAddr] userAddr collateral (return tx)
  void $ eaSubmitTx $ Wallet.signTx txBody [userKey, colKey]

  return $
    CarbonMintResponse
      ipfsAddResp.ipfs_hash
      ipfsAddResp.name
      ipfsAddResp.size
      ipfsPinObjResp.state
      carbonNftAsset
      (txBodySubmitTxResponse txBody)
