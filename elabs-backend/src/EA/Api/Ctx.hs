module EA.Api.Ctx (
  runGYTxMonadNodeF,
  runSkeletonF,
) where

import GeniusYield.Transaction
import GeniusYield.TxBuilder
import GeniusYield.Types

runSkeletonF ::
  GYNetworkId ->
  GYProviders ->
  [GYAddress] ->
  GYAddress ->
  Maybe (GYTxOutRef, Bool) ->
  GYTxBuilderMonadIO (GYTxSkeleton v) ->
  IO GYTxBody
runSkeletonF = runGYTxMonadNodeF GYRandomImproveMultiAsset

runGYTxMonadNodeF :: GYCoinSelectionStrategy -> GYNetworkId -> GYProviders -> [GYAddress] -> GYAddress -> Maybe (GYTxOutRef, Bool) -> GYTxBuilderMonadIO (GYTxSkeleton v) -> IO GYTxBody
runGYTxMonadNodeF strat nid providers addrs change collateral act = runGYTxBuilderMonadIO nid providers addrs change collateral $ act >>= buildTxBodyWithStrategy strat
