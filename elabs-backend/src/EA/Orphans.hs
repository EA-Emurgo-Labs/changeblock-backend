{-# LANGUAGE NoOverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module EA.Orphans (
  MultipartFormDataTmp,
  GYPaymentKeyHash,
) where

import Cardano.Api qualified as Api
import Control.Lens ((.~), (?~))
import Data.Aeson
import Data.Swagger (
  ParamAnySchema (ParamOther),
  ParamLocation (ParamFormData),
  SwaggerType (..),
  toParamSchema,
 )
import Data.Swagger.Lens
import Data.Text qualified as T
import Database.Persist (PersistField, PersistValue, SqlType (SqlString))
import Database.Persist.Class.PersistField (PersistField (fromPersistValue))
import Database.Persist.Postgresql (PersistFieldSql (sqlType))
import Database.Persist.Sql (PersistField (toPersistValue))
import EA.Api.Types (CarbonMintRequest (..), UserId (..))
import EA.Internal (addConsumes, addDescription, addParam)
import GeniusYield.Types (GYPaymentKeyHash, paymentKeyHashToApi)
import Servant (Tagged, (:>))
import Servant.Multipart (MultipartData, MultipartForm, Tmp)
import Servant.Swagger (HasSwagger (..))

--------------------------------------------------------------------------------

type MultipartFormDataTmp = MultipartForm Tmp (MultipartData Tmp)

instance
  (HasSwagger api) =>
  HasSwagger (Tagged CarbonMintRequest MultipartFormDataTmp :> api)
  where
  toSwagger _ =
    toSwagger (Proxy :: Proxy api)
      & addConsumes ["multipart/form-data"]
      & addParam fileParam
      & addParam jsonParam
      & addDescription
        ( "This call creates new carbon tokens for the user, creates a buy "
            <> "order, and creates an NFT for the user."
        )
    where
      fileParam =
        mempty
          & name .~ "file"
          & required ?~ True
          & description ?~ "File to upload"
          & schema
            .~ ParamOther
              ( mempty
                  & in_ .~ ParamFormData
                  & paramSchema .~ (mempty & type_ ?~ SwaggerFile)
              )

      jsonParam =
        mempty
          & name .~ "data"
          & required ?~ True
          & description ?~ "JSON data"
          & schema
            .~ ParamOther
              ( mempty
                  & in_ .~ ParamFormData
                  & allowEmptyValue ?~ False
                  & paramSchema
                    .~ ( toParamSchema (Proxy :: Proxy String)
                          & default_ ?~ toJSON carbonMintRequestExample
                       )
              )

carbonMintRequestExample :: CarbonMintRequest
carbonMintRequestExample = CarbonMintRequest (UserId 14) 100000 1000

--------------------------------------------------------------------------------

instance PersistField GYPaymentKeyHash where
  toPersistValue =
    (toPersistValue :: String -> PersistValue)
      . T.unpack
      . Api.serialiseToRawBytesHexText
      . paymentKeyHashToApi

  fromPersistValue x = case (fromPersistValue x :: Either Text String) of
    Left err -> Left $ T.replace "String" "GYPaymentKeyHash" err
    Right pkh -> Right $ fromString pkh

instance PersistFieldSql GYPaymentKeyHash where
  sqlType _ = SqlString
