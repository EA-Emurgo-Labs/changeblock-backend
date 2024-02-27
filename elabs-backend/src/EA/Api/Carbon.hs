module EA.Api.Carbon (
  CarbonApi,
  CarbonMintRequest (..),
  handleCarbonApi,
)
where

import Data.Aeson qualified as Aeson
import Data.Maybe (fromJust)
import Data.Swagger qualified as Swagger
import Data.Text qualified as T
import Data.Text.Encoding.Base16 (encodeBase16)
import EA (
  EAApp,
  EAAppEnv (eaAppEnvEscrowAddr, eaAppEnvGYNetworkId, eaAppEnvGYProviders, eaAppEnvMarketplaceVersion, eaAppEnvOracleNFTTokenName, eaAppEnvScripts),
  eaAppEnvOracleNFTOutRef,
  eaAppEnvOracleOperatorAddr,
  eaLiftEither,
  eaLiftMaybe,
  eaSubmitTx,
 )
import EA.Api.Types (SubmitTxResponse, UserId, txBodySubmitTxResponse)
import EA.Script (marketplaceValidator, nftMintingPolicy, oracleValidator)
import EA.Script.Marketplace (MarketplaceParams (..))
import EA.Tx.Changeblock.MintIpfsNftCarbonToken (mintIpfsNftCarbonToken)
import EA.Wallet (
  eaGetAddresses,
  eaGetCollateralFromInternalWallet,
  eaGetInternalAddresses,
  eaSelectOref,
 )
import GeniusYield.TxBuilder (
  runGYTxMonadNode,
  runGYTxQueryMonadNode,
  scriptAddress,
 )
import GeniusYield.Types (
  GYAssetClass (GYToken),
  addressFromBech32,
  mintingPolicyId,
  unsafeTokenNameFromHex,
  validatorHash,
 )
import GeniusYield.Types.Address (addressToPubKeyHash)
import Internal.Ipfs (ipfsAddFile, ipfsPinObject)
import Internal.Ipfs.Types (IpfsAddResponse (..), IpfsPin (..))
import Internal.Wallet qualified as Wallet
import Servant (Header, JSON, Post, type (:>))
import Servant.Multipart (
  MultipartData,
  MultipartForm,
  Tmp,
  lookupFile,
  lookupInput,
 )
import Servant.Swagger (HasSwagger (toSwagger))

--------------------------------------------------------------------------------

type CarbonApi = CarbonMint

type CarbonMint =
  "carbon"
    :> MultipartForm Tmp (MultipartData Tmp)
    :> "mint"
    :> Post '[JSON] CarbonMintResponse

--------------------------------------------------------------------------------
-- FIXME: This is because of MultiparForm, which is not supported by HasSwagger

type CarbonMintFix =
  "carbon"
    :> Header "user_id" UserId
    :> "mint"
    :> Post '[JSON] CarbonMintResponse

instance {-# OVERLAPPING #-} HasSwagger CarbonApi where
  toSwagger _ = toSwagger (Proxy :: Proxy CarbonMintFix)

--------------------------------------------------------------------------------

data CarbonMintRequest = CarbonMintRequest
  { userId :: !UserId
  -- ^ The user ID.
  , amount :: !Natural
  -- ^ The amount of carbon to mint.
  , sell :: !Natural
  -- ^ The sell price per unit of carbon.
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON, Swagger.ToSchema)

data CarbonMintResponse = CarbonMintResponse
  { ipfsHash :: !Text
  , ipfsName :: !Text
  , ipfsSize :: !Text
  , ipfsPinningState :: !Text
  , submitTxInfo :: !SubmitTxResponse
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.ToJSON, Swagger.ToSchema)

--------------------------------------------------------------------------------

handleCarbonApi ::
  MultipartData Tmp ->
  EAApp CarbonMintResponse
handleCarbonApi multipartData = do
  filePart <- eaLiftEither id $ lookupFile "file" multipartData
  dataPart <- eaLiftEither id $ lookupInput "data" multipartData

  request <-
    eaLiftMaybe "Cannot decode JSON data" $
      Aeson.decode @CarbonMintRequest $
        encodeUtf8 dataPart

  nid <- asks eaAppEnvGYNetworkId
  providers <- asks eaAppEnvGYProviders
  scripts <- asks eaAppEnvScripts

  -- Get the internal address pairs.
  internalAddrPairs <- eaGetInternalAddresses False

  -- Get the user address. We don't need the signing key here.
  (userAddr, _) <-
    eaLiftMaybe "No addresses found"
      . listToMaybe
      =<< eaGetAddresses (userId request)

  -- Get the collateral address and its signing key.
  (collateral, colKey) <-
    eaGetCollateralFromInternalWallet >>= eaLiftMaybe "No collateral found"

  (addr, key, oref) <-
    eaSelectOref
      internalAddrPairs
      (\r -> collateral /= Just (r, True))
      >>= eaLiftMaybe "No UTxO found"

  issuer <- eaLiftMaybe "Cannot decode address" (addressToPubKeyHash addr)

  -- Get oracle NFT
  oracleNftOref <- asks eaAppEnvOracleNFTOutRef
  oracleNftTokenName <- asks eaAppEnvOracleNFTTokenName
  eaAppEnvOracleOperatorAddr <- asks eaAppEnvOracleOperatorAddr
  escrowAddr <- asks eaAppEnvEscrowAddr

  -- Get marketplace version
  marketplaceVersion <- asks eaAppEnvMarketplaceVersion

  -- Get oracle validator hash
  let oracleNftAsset = mintingPolicyId $ nftMintingPolicy oracleNftOref scripts
      orcAssetClass = GYToken oracleNftAsset oracleNftTokenName
      orcValidatorHash =
        validatorHash $ oracleValidator orcAssetClass (fromJust $ addressToPubKeyHash $ addressFromBech32 eaAppEnvOracleOperatorAddr) scripts

  let marketParams =
        MarketplaceParams
          { mktPrmOracleValidator = orcValidatorHash
          , mktPrmEscrowValidator = fromJust $ addressToPubKeyHash $ addressFromBech32 escrowAddr
          , -- \^ TODO: User proper pubkeyhash of escrow
            mktPrmVersion = marketplaceVersion
          , -- \^ It can be any string for now using v1.0.0
            mktPrmOracleSymbol = oracleNftAsset
          , mktPrmOracleTokenName = oracleNftTokenName
          }

  ipfsAddResp <- ipfsAddFile filePart
  ipfsPinObjResp <- ipfsPinObject ipfsAddResp.ipfs_hash

  marketplaceAddress <-
    liftIO $
      runGYTxQueryMonadNode nid providers $
        scriptAddress (marketplaceValidator marketParams scripts)

  let
    tokenName =
      unsafeTokenNameFromHex $
        encodeBase16 $
          T.take 10 $
            T.append "CBLK" ipfsAddResp.ipfs_hash

    tx =
      mintIpfsNftCarbonToken
        oref
        marketplaceAddress
        userAddr
        issuer
        tokenName
        (toInteger $ sell request)
        (toInteger $ amount request)
        scripts

  txBody <-
    liftIO $
      runGYTxMonadNode nid providers [addr] addr collateral (return tx)

  void $ eaSubmitTx $ Wallet.signTx txBody [key, colKey]

  return $
    CarbonMintResponse
      ipfsAddResp.ipfs_hash
      ipfsAddResp.name
      ipfsAddResp.size
      ipfsPinObjResp.state
      (txBodySubmitTxResponse txBody)
