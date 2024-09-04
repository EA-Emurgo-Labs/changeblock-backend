module EA.Api.Wallet (
  WalletApi,
  handleWalletApi,
  handleWalletBalanceApi,
) where

import Servant (Capture, GenericMode ((:-)), Get, HasServer (ServerT), JSON, NamedRoutes, QueryParam, type (:>))

import EA (EAApp, eaGetAddressValue', eaMarketplaceAddress)
import EA.Api.Types (UserId, WalletResponse (WalletResponse), WalletValueResp (WalletValueResp), walletAddressWithPubKeyHash)
import EA.Script.Marketplace (MarketplaceDatum (..), MarketplaceInfo (..), MarketplaceOrderType (M_BUY), marketplaceDatumToInfo)
import EA.Wallet (eaGetAddresses)
import GeniusYield.TxBuilder (addressToPubKeyHashIO, utxoDatumPure)
import GeniusYield.Types
import Internal.AdaPrice (getAdaPrice)

--------------------------------------------------------------------------------
data WalletApi mode = WalletApi
  { getWalletAddress :: mode :- WalletResp
  , getWalletBalance :: mode :- WalletBalance
  }
  deriving stock (Generic)

handleWalletApi :: ServerT (NamedRoutes WalletApi) EAApp
handleWalletApi =
  WalletApi
    { getWalletAddress = handleWalletAddressApi
    , getWalletBalance = handleWalletBalanceApi
    }

type WalletResp =
  "wallet"
    :> Capture "user" UserId
    :> Get '[JSON] WalletResponse

type WalletBalance =
  "wallet"
    :> Capture "user" UserId
    :> "balance"
    :> QueryParam "onlyCarbonToken" Bool
    :> Get '[JSON] WalletValueResp

handleWalletAddressApi :: UserId -> EAApp WalletResponse
handleWalletAddressApi userid = do
  addrs <- eaGetAddresses userid
  return $ WalletResponse (map (walletAddressWithPubKeyHash . fst) addrs) userid

handleWalletBalanceApi :: UserId -> Maybe Bool -> EAApp WalletValueResp
handleWalletBalanceApi userid mOnlyCarbonToken = do
  userAddrsWithPaymentKey <- eaGetAddresses userid
  let userAddrs = map fst userAddrsWithPaymentKey
  value <- case userAddrs of
    [] -> return $ valueFromList []
    (addr : _) -> do
      mktPlaceAddr <- eaMarketplaceAddress
      owner <- liftIO $ addressToPubKeyHashIO addr
      eaGetAddressValue' [addr, mktPlaceAddr] $ \u@(utxo, _) ->
        if utxoAddress utxo == mktPlaceAddr
          then handleMarketplaceUtxoValue (paymentKeyHashFromApi $ pubKeyHashToApi owner) u
          else checkOnlyAdaValue mOnlyCarbonToken $ utxoValue utxo

  adaPrice <- liftIO getAdaPrice
  let totalAdaValueUsd = calcTotAdaPrice value =<< adaPrice

  return $ WalletValueResp value adaPrice totalAdaValueUsd
  where
    checkOnlyAdaValue :: Maybe Bool -> GYValue -> GYValue
    checkOnlyAdaValue (Just True) val =
      let totAda = valueAssetClass val GYLovelace
       in valueSingleton GYLovelace totAda
    checkOnlyAdaValue _ val = val

    calcTotAdaPrice :: GYValue -> Double -> Maybe Double
    calcTotAdaPrice value adaPrice =
      let adaAmt = fst $ valueSplitAda value
       in Just $ (fromIntegral adaAmt / 1000000) * adaPrice

handleMarketplaceUtxoValue :: GYPaymentKeyHash -> (GYUTxO, Maybe GYDatum) -> GYValue
handleMarketplaceUtxoValue owner utxoWithDatum@(utxo, _) =
  case utxoDatumPure @MarketplaceDatum utxoWithDatum of
    Left _ -> valueFromList []
    Right (addr, val, datum) ->
      handle (marketplaceDatumToInfo (utxoRef utxo) val addr datum Nothing) val
  where
    handle :: Either String MarketplaceInfo -> GYValue -> GYValue
    handle (Left _) _ = valueFromList []
    handle (Right MarketplaceInfo {..}) val =
      if mktInfoOwner == owner && mktInfoIsSell == M_BUY
        then
          let carbonTokenAsset = GYToken mktInfoCarbonPolicyId mktInfoCarbonAssetName
           in valueSingleton carbonTokenAsset $ valueAssetClass val carbonTokenAsset
        else valueFromList []