module EA.Api.Mint (
  MintApi,
  handleOneShotMintByUserId,
  handleOneShotMintByWallet,
)
where

import EA (
  EAApp,
  EAAppEnv (..),
  eaLiftMaybe,
  eaSubmitTx,
  getOneShotMintingPolicy,
 )
import EA.Api.Types (
  SubmitTxResponse,
  UnsignedTxResponse,
  UserId,
  WalletParams (..),
  txBodySubmitTxResponse,
  unSignedTxWithFee,
 )
import EA.Tx.OneShotMint qualified as Tx
import EA.Wallet (
  eaGetAddresses,
  eaGetCollateralFromInternalWallet,
 )
import GeniusYield.GYConfig (GYCoreConfig (cfgNetworkId))
import GeniusYield.TxBuilder (runGYTxMonadNode)
import GeniusYield.Types (
  GYAddress,
  GYProviders,
  GYTxOutRef,
  GYTxOutRefCbor (getTxOutRefHex),
  gyQueryUtxosAtAddresses,
  randomTxOutRef,
 )
import Internal.Wallet (PaymentKey, signTx)
import Servant (Capture, JSON, Post, ReqBody, (:<|>), type (:>))

type MintApi = OneShotMintByWallet :<|> OneShotMintByUserId

type OneShotMintByWallet =
  "one-shot-mint"
    :> ReqBody '[JSON] WalletParams
    :> Post '[JSON] UnsignedTxResponse

type OneShotMintByUserId =
  "one-shot-mint"
    :> Capture "user" UserId
    :> Post '[JSON] SubmitTxResponse

selectOref ::
  GYProviders ->
  [(GYAddress, PaymentKey)] ->
  IO (Maybe (GYAddress, PaymentKey, GYTxOutRef))
selectOref _ [] = return Nothing
selectOref providers ((addr, key) : pairs) = do
  utxos <- gyQueryUtxosAtAddresses providers [addr]
  moref <- randomTxOutRef utxos
  case moref of
    Nothing -> selectOref providers pairs
    Just (oref, _) -> return $ Just (addr, key, oref)

handleOneShotMintByUserId :: UserId -> EAApp SubmitTxResponse
handleOneShotMintByUserId userId = do
  nid <- asks (cfgNetworkId . eaAppEnvGYCoreConfig)
  providers <- asks eaAppEnvGYProviders

  pairs <- eaGetAddresses userId
  (addr, key, oref) <-
    liftIO (selectOref providers pairs) >>= eaLiftMaybe "No UTxO found"

  policy <- getOneShotMintingPolicy oref

  eaGetCollateralFromInternalWallet >>= \case
    Nothing -> eaLiftMaybe "No collateral found" Nothing
    Just (collateral, colKey) -> do
      txBody <-
        liftIO $
          runGYTxMonadNode
            nid
            providers
            [addr]
            addr
            collateral
            (return $ Tx.oneShotMint addr oref 1 policy)
      void $ eaSubmitTx $ signTx txBody [colKey, key]
      return $ txBodySubmitTxResponse txBody

handleOneShotMintByWallet :: WalletParams -> EAApp UnsignedTxResponse
handleOneShotMintByWallet WalletParams {..} = do
  nid <- asks (cfgNetworkId . eaAppEnvGYCoreConfig)
  providers <- asks eaAppEnvGYProviders
  utxos <- liftIO $ gyQueryUtxosAtAddresses providers usedAddrs

  (oref, _) <-
    liftIO (randomTxOutRef utxos) >>= eaLiftMaybe "No UTxO found"

  policy <- getOneShotMintingPolicy oref

  addr <- eaLiftMaybe "No address provided" $ viaNonEmpty head usedAddrs

  txBody <-
    liftIO $
      runGYTxMonadNode
        nid
        providers
        [addr]
        addr
        ( collateral
            >>= ( \c ->
                    Just
                      (getTxOutRefHex c, True)
                )
        )
        (return $ Tx.oneShotMint addr oref 1 policy)

  pure $ unSignedTxWithFee txBody
