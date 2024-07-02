module EA.Api.Orders.BuyOrderTests (tests) where

import Data.Aeson qualified as Aeson
import Data.List.NonEmpty qualified as NE
import EA (eaLiftMaybe, eaMarketplaceInfos, runEAApp)
import EA.Api.Order.Types (OrderBuyRequest (OrderBuyRequest))
import EA.Api.Types (UserId (UserId))
import EA.Script.Marketplace (MarketplaceInfo (mktInfoAmount, mktInfoIsSell, mktInfoTxOutRef), MarketplaceOrderType (M_SELL))
import EA.Test.Helpers qualified as Helpers
import EA.Wallet (eaGetAddresses)
import GeniusYield.TxBuilder (addressToPubKeyHashIO)
import GeniusYield.Types (paymentKeyHashFromApi, pubKeyHashToApi, valueFromLovelace)
import Network.HTTP.Types (methodPost)
import Setup (EACtx (..), checkResponseTxConfirmed, sendFundsToAddress, server, withEaCtx)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps)
import Test.Tasty.Wai (SResponse (simpleBody), assertStatus, runSession)

tests :: IO EACtx -> TestTree
tests eaCtx =
  testGroup
    "Order Buy Tests"
    [ testCaseSteps "Test /api/v0/orders/buy endpoint" $
        \step -> withEaCtx eaCtx $
          \EACtx {..} -> do
            (buyerAddr, _) <- runEAApp eaCtxEnv $ do
              addrs <- eaGetAddresses (UserId 3)
              pure $ head $ NE.fromList addrs

            -- Sending 100 ADA to the buyer
            void $ sendFundsToAddress buyerAddr (valueFromLovelace 100_000_000) eaCtxCtx

            buyerPubkeyHash <- addressToPubKeyHashIO buyerAddr
            let buyerPaymentKeyHash = paymentKeyHashFromApi $ pubKeyHashToApi buyerPubkeyHash

            (utxoRef, qty) <- runEAApp eaCtxEnv $ do
              mInfos <- eaMarketplaceInfos eaCtxMarketplaceParams
              sellInfo <- eaLiftMaybe "No sell order found" $ find (\mi -> mktInfoIsSell mi == M_SELL) mInfos

              let qty = if mktInfoAmount sellInfo > 100 then mktInfoAmount sellInfo `div` 10 else mktInfoAmount sellInfo
              pure (mktInfoTxOutRef sellInfo, fromInteger qty)

            let jsonData = Aeson.encode $ OrderBuyRequest buyerPaymentKeyHash qty utxoRef

            -- Sending some fund to the buyer
            step "Sending POST request to /api/v0/orders/buy"

            response <- flip runSession (server eaCtxEnv) $ do
              let token = encodeUtf8 eaCtxToken
              response <-
                Helpers.request
                  methodPost
                  "/api/v0/orders/buy"
                  jsonData
                  [ ("Content-type", "Application/json")
                  , ("Authorization", token)
                  ]

              assertStatus 200 response

              pure response

            void $ checkResponseTxConfirmed eaCtxCtx (simpleBody response)
    ]