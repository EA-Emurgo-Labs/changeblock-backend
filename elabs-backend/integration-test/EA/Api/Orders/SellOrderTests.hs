module EA.Api.Orders.SellOrderTests (tests) where

import Data.Aeson qualified as Aeson
import Data.List.NonEmpty qualified as NE
import EA (eaLiftMaybe, eaMarketplaceInfos, runEAApp)
import EA.Api.Order.Types (OrderSellRequest (OrderSellRequest))
import EA.Api.Types (UserId (UserId))
import EA.Script.Marketplace (MarketplaceInfo (mktInfoIsSell, mktInfoOwner, mktInfoTxOutRef), MarketplaceOrderType (M_BUY))
import EA.Test.Helpers qualified as Helpers
import EA.Wallet (eaGetAddresses)
import GeniusYield.TxBuilder (addressToPubKeyHashIO)
import GeniusYield.Types (valueFromLovelace)
import Network.HTTP.Types (methodPost)
import Setup (EACtx (..), checkResponseTxConfirmed, sendFundsToAddress, server, withEaCtx)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps)
import Test.Tasty.Wai (SResponse (simpleBody), assertStatus, runSession)

tests :: IO EACtx -> TestTree
tests eaCtx =
  testGroup
    "Order Buy Tests"
    [ testCaseSteps "Test /api/v0/orders/create_sell_order endpoint" $
        \step -> withEaCtx eaCtx $
          \EACtx {..} -> do
            (ownerAddr, _) <- runEAApp eaCtxEnv $ do
              addrs <- eaGetAddresses (UserId 1)
              pure $ head $ NE.fromList addrs

            ownerPubkeyHash <- addressToPubKeyHashIO ownerAddr

            -- Sending 100 ADA to the owner
            void $ sendFundsToAddress ownerAddr (valueFromLovelace 100_000_000) eaCtxCtx

            utxoRef <- runEAApp eaCtxEnv $ do
              mInfos <- eaMarketplaceInfos eaCtxMarketplaceParams
              sellInfo <- eaLiftMaybe "No sell order found" $ find (\mi -> mktInfoIsSell mi == M_BUY && mktInfoOwner mi == ownerPubkeyHash) mInfos

              pure $ mktInfoTxOutRef sellInfo

            let jsonData = Aeson.encode $ OrderSellRequest ownerPubkeyHash 500 100 utxoRef

            -- Sending some fund to the buyer
            step "Sending POST request to /api/v0/orders/create_sell_order"

            resp <- flip runSession (server eaCtxEnv) $ do
              let token = encodeUtf8 eaCtxToken
              response <-
                Helpers.request
                  methodPost
                  "/api/v0/orders/update-sale-price"
                  jsonData
                  [ ("Content-type", "Application/json")
                  , ("Authorization", token)
                  ]

              assertStatus 200 response

              pure response

            void $ checkResponseTxConfirmed eaCtxCtx (simpleBody resp)
    ]