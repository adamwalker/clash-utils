module SHA3Spec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable, toList, sampleN, fromList, type (+), System)
import Test.Hspec
import Test.QuickCheck

import Data.Word
import Data.Maybe
import Data.List (find)
import Crypto.Hash
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import Clash.Crypto.SHA3

spec :: SpecWith ()
spec = 
    describe "SHA3" $ do
        it "224" $ property prop_224
        it "256" $ property prop_256
        it "384" $ property prop_384
        it "512" $ property prop_512

prop :: (((n + 1) + n0) ~ 25, KnownNat n0) => ([Word8] -> [Word8]) -> Int -> Vec n (BitVector 64) -> Bool
prop hashFunc mdlen vec =
    let 
        starts = fromList $ False : True : repeat False
        res    = fmap snd $ find fst $ sampleN @System 30 $ bundle $ sha3 starts (pure (vec :< 0x8000000000000006))
        resStr = take mdlen $ concat $ map (reverse . toList . (unpack :: BitVector 64 -> Vec 8 (Word8))) $ toList $ Clash.concat $ fromJust res
        str    = concat $ map (reverse . toList . (unpack :: BitVector 64 -> Vec 8 (Word8))) $ toList vec
        expect = hashFunc str
    in resStr == expect

prop_224 :: Vec 17 (BitVector 64) -> Bool
prop_224 = prop (\x -> BA.unpack (hash (BS.pack x) :: Digest SHA3_224)) 28

prop_256 :: Vec 16 (BitVector 64) -> Bool
prop_256 = prop (\x -> BA.unpack (hash (BS.pack x) :: Digest SHA3_256)) 32

prop_384 :: Vec 12 (BitVector 64) -> Bool
prop_384 = prop (\x -> BA.unpack (hash (BS.pack x) :: Digest SHA3_384)) 48

prop_512 :: Vec 8 (BitVector 64) -> Bool
prop_512 = prop (\x -> BA.unpack (hash (BS.pack x) :: Digest SHA3_512)) 64

