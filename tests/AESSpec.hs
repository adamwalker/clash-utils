module AESSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable, toList, sampleN, fromList, System)
import Test.Hspec
import Test.QuickCheck

import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Crypto.Error
import Clash.Crypto.AES

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.List as Prelude

spec :: SpecWith ()
spec = do
    describe "AES" $ 
        it "encrypts" $ property prop_AES

prop_AES :: BitVector 128 -> BitVector 128 -> Property
prop_AES key block = fmap (fmap toBS) res === Just (True, expect)
    where
    toBS x  = BS.pack $ Prelude.map fromIntegral $ toList $ (unpack x :: Vec 16 (BitVector 8))
    keyBS   = toBS key
    blockBS = toBS block
    expect  = ecbEncrypt (throwCryptoError $ cipherInit keyBS :: AES128) blockBS
    res     = Prelude.find fst $ sampleN @System 20 (aesEncrypt starts (pure key) (pure block)) 
    starts  = fromList $ False : True : Prelude.repeat False

