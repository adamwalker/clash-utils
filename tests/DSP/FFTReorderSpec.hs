module DSP.FFTReorderSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable, System, sample, fromList)
import Test.Hspec
import Test.QuickCheck hiding (sample)

import Clash.DSP.FFT.Reorder
import Clash.Misc

--serial FFT
spec = describe "FFT reordering" $ do
    specify "Serial bit reversal reorder gives correct result" $ property prop_fftBitReversalReorder

goldenBitReversalReorder 
    :: forall n a
    .  SNat n
    -> [a]
    -> [a]
goldenBitReversalReorder SNat inp = map ((inp !!) . fromIntegral) indices
    where
    indices :: [BitVector n]
    indices = map revBV [0..]

prop_fftBitReversalReorder :: Property
prop_fftBitReversalReorder 
    = forAll (listOf (vectorOf 64 arbitrary)) $ \(vec :: [[Int]]) -> 
        let result = sample @System $ bitReversalReorder (SNat @ 6) (pure True) $ fromList (0 : concat vec ++ repeat 0)
            expect = concatMap (goldenBitReversalReorder (SNat @ 6)) vec
        in take (64 * length vec) (drop 66 result) === expect

