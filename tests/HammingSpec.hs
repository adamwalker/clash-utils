module HammingSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable)
import Test.Hspec
import Test.QuickCheck

import Clash.ErrorControl.Hamming

--Hamming codes
--Test the (15, 11) code

spec = describe "Hamming encoding/decoding" $ do
    specify "decode inverse encode" $ property prop_hamming

hammingGen :: Vec 11 (BitVector 4)
hammingGen = Clash.map pack $ Clash.unconcatI $ $(listToVecTH $ concat $ take 11 $ map (take 4) generator)

prop_hamming :: Index 15 -> BitVector 11 -> Bool
prop_hamming mutIdx dat = dat == corrected
    where
    --encode
    parityBits  = hammingParity hammingGen dat
    encoded     = dat ++# parityBits
    --flip a single bit
    mutated     = Clash.complementBit encoded (fromIntegral mutIdx)
    --decode
    corrected   = correctError hammingGen (Clash.slice (SNat @ 3) (SNat @ 0) mutated) (Clash.slice (SNat @ 14) (SNat @ 4) mutated)

