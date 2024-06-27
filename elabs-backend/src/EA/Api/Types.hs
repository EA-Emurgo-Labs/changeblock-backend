module EA.Api.Types (
  AuthorizationHeader (..),
  UserId (..),
  SubmitTxParams (..),
  SubmitTxResponse (..),
  WalletParams (..),
  UnsignedTxResponse (..),
  WalletResponse (..),
  WalletValueResp (..),
  CarbonMintRequest (..),
  txBodySubmitTxResponse,
  unSignedTxWithFee,
  walletAddressWithPubKeyHash,
) where

import Data.Aeson qualified as Aeson
import Data.Swagger qualified as Swagger
import Data.Text qualified as T
import Data.Text.Class qualified as TC

import GeniusYield.Types (
  GYAddress,
  GYAddressBech32,
  GYPubKeyHash,
  GYTx,
  GYTxBody,
  GYTxId,
  GYTxOutRefCbor,
  GYTxWitness,
  GYValue,
  addressToBech32,
  addressToPubKeyHash,
  txBodyFee,
  txBodyTxId,
  txToHex,
  unsignedTx,
 )
import Servant (FromHttpApiData (parseUrlPiece), ToHttpApiData (toUrlPiece))

import Database.Persist (
  PersistField (fromPersistValue),
  PersistValue,
  SqlType (SqlInt64),
 )
import Database.Persist.Class (PersistField (toPersistValue))
import Database.Persist.Sql (PersistFieldSql (sqlType))

--------------------------------------------------------------------------------

data SubmitTxParams = SubmitTxParams
  { txUnsigned :: !GYTx
  , txWit :: !GYTxWitness
  }
  deriving stock (Generic)
  deriving anyclass (Aeson.FromJSON, Swagger.ToSchema)

data SubmitTxResponse = SubmitTxResponse
  { submitTxFee :: !Integer
  , submitTxId :: !GYTxId
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.ToJSON, Swagger.ToSchema, Aeson.FromJSON)

txBodySubmitTxResponse :: GYTxBody -> SubmitTxResponse
txBodySubmitTxResponse txBody =
  SubmitTxResponse
    { submitTxFee = txBodyFee txBody
    , submitTxId = txBodyTxId txBody
    }

data WalletParams = WalletParams
  { usedAddrs :: ![GYAddress]
  , changeAddr :: !GYAddress
  , collateral :: !(Maybe GYTxOutRefCbor)
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON, Swagger.ToSchema)

data UnsignedTxResponse = UnsignedTxResponse
  { txBodyHex :: !T.Text
  , txFee :: !(Maybe Integer)
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON, Swagger.ToSchema)

unSignedTxWithFee :: GYTxBody -> UnsignedTxResponse
unSignedTxWithFee txBody =
  UnsignedTxResponse
    { txBodyHex = T.pack $ txToHex $ unsignedTx txBody
    , txFee = Just $ txBodyFee txBody
    }

data CarbonMintRequest = CarbonMintRequest
  { userId :: !UserId
  -- ^ The user ID.
  , amount :: !Natural
  -- ^ The amount of carbon to mint.
  , sell :: !Natural
  -- ^ The sell price per unit of carbon.
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON, Swagger.ToSchema)

--------------------------------------------------------------------------------
-- UserId

newtype UserId = UserId {unUserId :: Natural}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Swagger.ToSchema, Swagger.ToParamSchema)

instance Aeson.FromJSON UserId where
  parseJSON = fmap UserId . Aeson.parseJSON

instance Aeson.ToJSON UserId where
  toJSON = Aeson.toJSON . unUserId

instance FromHttpApiData UserId where
  parseUrlPiece = bimap (T.pack . TC.getTextDecodingError) UserId . TC.fromText

instance ToHttpApiData UserId where
  toUrlPiece = TC.toText . unUserId

-- Check OverflowNatural documentation
instance PersistField UserId where
  toPersistValue =
    (toPersistValue :: Int64 -> PersistValue) . fromIntegral . unUserId
  fromPersistValue x = case (fromPersistValue x :: Either Text Int64) of
    Left err -> Left $ T.replace "Int64" "UserId" err
    Right int -> Right $ UserId $ fromIntegral int

instance PersistFieldSql UserId where
  sqlType _ = SqlInt64

--------------------------------------------------------------------------------
-- Wallet

data WalletResponse = WalletResponse
  { addresses :: ![WalletAddressWithPubKeyHash]
  , userId :: !UserId
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON, Swagger.ToSchema)

data WalletValueResp = WalletValueResp
  { value :: !GYValue
  , adaPrice :: !(Maybe Double)
  , totAdaValueUsd :: !(Maybe Double)
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON, Swagger.ToSchema)

instance Aeson.ToJSON WalletValueResp where
  toJSON WalletValueResp {..} =
    Aeson.object
      [ "value" Aeson..= value
      , "ada_price_usd" Aeson..= adaPrice
      , "tot_ada_value_usd" Aeson..= totAdaValueUsd
      ]
data WalletAddressWithPubKeyHash = WalletResponseWithPubKeyHash
  { address :: !GYAddressBech32
  , pubKeyHash :: !(Maybe GYPubKeyHash)
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON, Swagger.ToSchema)

walletAddressWithPubKeyHash :: GYAddress -> WalletAddressWithPubKeyHash
walletAddressWithPubKeyHash addr = WalletResponseWithPubKeyHash (addressToBech32 addr) (addressToPubKeyHash addr)

--------------------------------------------------------------------------------
-- Headers

data AuthorizationHeader = AuthorizationHeader
  { unAuthorizationHeader :: T.Text
  }
  deriving stock (Show, Generic)
  deriving anyclass
    ( Aeson.FromJSON
    , Aeson.ToJSON
    , Swagger.ToSchema
    , Swagger.ToParamSchema
    )

instance FromHttpApiData AuthorizationHeader where
  parseUrlPiece = Right . AuthorizationHeader

instance ToHttpApiData AuthorizationHeader where
  toUrlPiece = unAuthorizationHeader
