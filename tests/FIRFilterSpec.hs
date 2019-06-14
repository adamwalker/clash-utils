module FIRFilterSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockReset, type (+), extend, Undefined)
import Control.Applicative
import Test.Hspec
import Test.QuickCheck

import qualified Data.List.Split as S
import Clash.DSP.FIRFilter

--FIR filter testing
--Check that both optimised filters return the same results as the unoptimised one
spec = describe "FIR filters" $ do
    describe "Parallel" $ do
        specify "transposed"          $ property $ prop_FilterTransposed
        specify "systolic"            $ property $ prop_FilterSystolic
        specify "systolic symmetric"  $ property $ prop_FilterSystolicSymmetric
        specify "symmetric"           $ property $ prop_FilterSymmetric
        specify "systolic symmetric"  $ property $ prop_systolicSymmetric
        specify "systolic half band"  $ property $ prop_systolicHalfBand
    describe "Semi-parallel" $ do
        specify "semi parallel 1"     $ property $ prop_semiParallelFIR1
        specify "semi parallel 2"     $ property $ prop_semiParallelFIR2
        specify "semi parallel 3"     $ property $ prop_semiParallelFIR3

goldenFIR 
    :: (HiddenClockReset dom gated sync, Num a, KnownNat n, Undefined a)
    => Vec (n + 1) a
    -> Signal dom Bool
    -> Signal dom a
    -> Signal dom a
goldenFIR = fir (*) (+)

macRealReal c i a = c * i + a

macPreAddRealReal c x y a = c * (x + y) + a

liftA4 f w x y z = f <$> w <*> x <*> y <*> z

prop_FilterTransposed :: Vec 16 (Signed 32) -> [Signed 32] -> Bool
prop_FilterTransposed coeffs input = 
       take (length input) (simulate (goldenFIR coeffs (pure True)) input) 
    == take (length input) (drop 1 (simulate (firTransposed (const (liftA3 macRealReal)) (Clash.reverse coeffs) (pure True)) input))

prop_FilterSystolic :: Vec 16 (Signed 32) -> [Signed 32] -> Bool
prop_FilterSystolic coeffs input = 
       take (length input) (simulate (goldenFIR coeffs (pure True)) input) 
    == take (length input) (drop 16 $ simulate (firSystolic (const (liftA3 macRealReal)) coeffs (pure True)) input)

prop_FilterSystolicSymmetric :: Vec 16 (Signed 32) -> [Signed 32] -> Bool
prop_FilterSystolicSymmetric coeffs input = 
       take (length input) (simulate (goldenFIR (coeffs Clash.++ Clash.reverse coeffs) (pure True)) input) 
    == take (length input) (drop 16 $ simulate (firSystolicSymmetric (const (liftA4 macPreAddRealReal)) coeffs (pure True)) input)

prop_FilterSymmetric :: Vec 16 (Signed 32) -> [Signed 32] -> Bool
prop_FilterSymmetric coeffs input = 
       take (length input) (simulate (Clash.register 0 . goldenFIR (Clash.reverse coeffs Clash.++ coeffs) (pure True)) input) 
    == take (length input) (simulate (firSymmetric (const (liftA4 macPreAddRealReal)) coeffs (pure True)) input)

prop_systolicSymmetric :: Vec 16 (Signed 32) -> Signed 32 -> [Signed 32] -> Bool
prop_systolicSymmetric coeffs mid input = 
       take (length input) (simulate (goldenFIR (coeffs Clash.++ Clash.singleton mid Clash.++ Clash.reverse coeffs) (pure True)) input)
    == take (length input) (drop 17 $ simulate (firSystolicSymmetricOdd (const (liftA4 macPreAddRealReal)) (coeffs Clash.++ Clash.singleton mid) (pure True)) input)

prop_systolicHalfBand :: Vec 16 (Signed 32) -> Signed 32 -> [Signed 32] -> Bool
prop_systolicHalfBand coeffs mid input = 
       take (length input) (simulate (goldenFIR (coeffs' Clash.++ Clash.singleton mid Clash.++ Clash.reverse coeffs') (pure True)) input)
    == take (length input) (drop 17 $ simulate (firSystolicHalfBand (const (liftA4 macPreAddRealReal)) (coeffs Clash.++ Clash.singleton mid) (pure True)) input)
    where
    coeffs' = Clash.init (Clash.merge coeffs (Clash.repeat 0))

--Semi-parallel FIR filter has lots of tests because it is confusing
prop_semiParallelFIR1 :: Vec 9 (Signed 32) -> [Signed 32] -> Bool
prop_semiParallelFIR1 coeffs input = res1 == res2
    where
    coeffs2 :: Vec 3 (Vec 3 (Signed 32))
    coeffs2 =  Clash.unconcatI coeffs
    input'  =  input ++ repeat 0
    res1    =  take 50 $ simulate (goldenFIR coeffs (pure True)) input'
    res2    =  take 50 $ map head $ S.chunksOf 3 $ drop 9 $ simulate (semiParallelFIR coeffs2 (pure True)) (concatMap ((++ [0, 0]) . pure) input' ++ repeat 0)

prop_semiParallelFIR2 :: Vec 15 (Signed 32) -> [Signed 32] -> Bool
prop_semiParallelFIR2 coeffs input = res1 == res2
    where
    coeffs2 :: Vec 5 (Vec 3 (Signed 32))
    coeffs2 =  Clash.unconcatI coeffs
    input'  =  input ++ repeat 0
    res1    =  take 50 $ simulate (goldenFIR coeffs (pure True)) input'
    res2    =  take 50 $ map head $ S.chunksOf 3 $ drop 11 $ simulate (semiParallelFIR coeffs2 (pure True)) (concatMap ((++ [0, 0]) . pure) input' ++ repeat 0)

prop_semiParallelFIR3 :: Vec 20 (Signed 32) -> [Signed 32] -> Bool
prop_semiParallelFIR3 coeffs input = res1 == res2
    where
    coeffs2 :: Vec 5 (Vec 4 (Signed 32))
    coeffs2 =  Clash.unconcatI coeffs
    input'  =  input ++ repeat 0
    res1    =  take 50 $ simulate (goldenFIR coeffs (pure True)) input'
    res2    =  take 50 $ map head $ S.chunksOf 4 $ drop 12 $ simulate (semiParallelFIR coeffs2 (pure True)) (concatMap ((++ [0, 0, 0]) . pure) input' ++ repeat 0)

