module Internal.Wallet (
  RootKey,
  PaymentKey,
  deriveAddress,
  readRootKey,
  writeRootKey,
  genRootKeyFromMnemonic,
  signTx,
)
where

import Cardano.Address (
  HasNetworkDiscriminant (NetworkDiscriminant),
  bech32,
 )
import Cardano.Address.Derivation (
  Depth (AccountK, PaymentK, RootK),
  DerivationType (Hardened, Soft),
  GenMasterKey (genMasterKeyFromMnemonic),
  HardDerivation (deriveAccountPrivateKey, deriveAddressPrivateKey),
  Index (indexToWord32),
  XPrv,
  indexFromWord32,
  toXPub,
  xprvFromBytes,
  xprvToBytes,
 )
import Cardano.Address.Style.Shelley (Shelley, getKey, liftXPrv)
import Cardano.Address.Style.Shelley qualified as S
import Cardano.Api.Shelley (
  SigningKey (PaymentExtendedSigningKey),
 )
import Cardano.Mnemonic (SomeMnemonic)
import Data.ByteString qualified as BS
import Data.Tagged (Tagged)
import GHC.Show (Show (show))
import GeniusYield.Types (
  GYAddress,
  GYExtendedPaymentSigningKey,
  GYNetworkId (GYMainnet),
  GYTx,
  GYTxBody,
  addressFromTextMaybe,
  extendedPaymentSigningKeyFromApi,
  signGYTxBody,
 )

--------------------------------------------------------------------------------

deriveAddress ::
  GYNetworkId ->
  RootKey ->
  Tagged "Account" Int64 ->
  Tagged "Address" Int64 ->
  Either String (GYAddress, PaymentKey)
deriveAddress nid (RootKey rootK) acc addr = do
  accI <-
    fromIntegralIndex acc
      >>= \index ->
        maybe
          (Left "Cannot create account index")
          Right
          ( indexFromWord32
              ( index
                  + indexToWord32 (minBound @(Index 'Hardened 'AccountK))
              )
          )
  addrI <-
    fromIntegralIndex addr
      >>= \index ->
        maybe
          (Left "Cannot create address index")
          Right
          ( indexFromWord32
              ( index
                  + indexToWord32 (minBound @(Index 'Soft 'PaymentK))
              )
          )

  let acctK = deriveAccountPrivateKey rootK accI
      addrK = deriveAddressPrivateKey acctK S.UTxOExternal addrI
      stakeK = S.deriveDelegationPrivateKey acctK
      paymentCredential = S.PaymentFromExtendedKey (toXPub <$> addrK)
      delegationCredential = S.DelegationFromExtendedKey (toXPub <$> stakeK)
      addr =
        S.delegationAddress (network nid) paymentCredential delegationCredential

  maybe
    (Left "Cannot decode beck to GYAddress")
    Right
    ( do
        gyAddr <- addressFromTextMaybe . bech32 $ addr
        return (gyAddr, PaymentKey addrK)
    )

fromIntegralIndex :: (Integral a) => a -> Either String Word32
fromIntegralIndex a
  | a < 0 = Left "fromIntegralIndex: negative"
  | a > 2147483647 = Left "fromIntegralIndex: too large"
  | otherwise = Right $ fromIntegral a

network :: GYNetworkId -> NetworkDiscriminant Shelley
network GYMainnet = S.shelleyMainnet
network _ = S.shelleyTestnet

--------------------------------------------------------------------------------
-- Payment key
--
-- Dont expose the internals of the payment key
-- Dont implement any instances

newtype PaymentKey = PaymentKey {_unPaymentKey :: Shelley 'PaymentK XPrv}

instance Show PaymentKey where
  show _ = "PaymentKey"

-- internal function, dont export
toShelleyWitnessSigningKey :: PaymentKey -> GYExtendedPaymentSigningKey
toShelleyWitnessSigningKey (PaymentKey key) =
  extendedPaymentSigningKeyFromApi $ PaymentExtendedSigningKey (getKey key)

signTx :: GYTxBody -> [PaymentKey] -> GYTx
signTx txBody skeys =
  let witnesss = map toShelleyWitnessSigningKey skeys
   in signGYTxBody txBody witnesss

--------------------------------------------------------------------------------
-- Root key
--
-- Dont expose the internals of the root key
-- Dont implement any instances

newtype RootKey = RootKey {_unRootKey :: Shelley 'RootK XPrv}

instance Show RootKey where
  show _ = "RootKey"

readRootKey :: FilePath -> IO (Maybe RootKey)
readRootKey fp = do
  bs <- BS.readFile fp
  return $ do
    key <- liftXPrv <$> xprvFromBytes bs
    return $ RootKey key

writeRootKey :: FilePath -> RootKey -> IO ()
writeRootKey fp (RootKey key) = BS.writeFile fp (xprvToBytes . getKey $ key)

genRootKeyFromMnemonic :: SomeMnemonic -> RootKey
genRootKeyFromMnemonic mw =
  RootKey $ genMasterKeyFromMnemonic @Shelley mw mempty
