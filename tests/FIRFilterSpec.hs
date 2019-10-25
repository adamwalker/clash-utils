module FIRFilterSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable, type (+), extend, NFDataX, System, fromList, singleton, sample)
import Control.Applicative
import Test.Hspec
import Test.QuickCheck hiding (sample)

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
    describe "Semi-parallel systolic" $ do
        specify "Semi-parallel 1"     $ property $ prop_semiParallelFIRSystolic
        specify "Semi-parallel 2"     $ property $ prop_semiParallelFIRSystolicMultiStage
    describe "Semi-parallel" $ do
        specify "semi parallel 1"     $ property $ prop_semiParallelFIR1
        specify "semi parallel 2"     $ property $ prop_semiParallelFIR2
        specify "semi parallel 3"     $ property $ prop_semiParallelFIR3

goldenFIR 
    :: (HiddenClockResetEnable dom, Num a, KnownNat n, NFDataX a)
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
       take (length input) (simulate @System (goldenFIR coeffs (pure True)) input) 
    == take (length input) (drop 1 (simulate @System (firTransposed (const (liftA3 macRealReal)) (Clash.reverse coeffs) (pure True)) input))

prop_FilterSystolic :: Vec 16 (Signed 32) -> [Signed 32] -> Bool
prop_FilterSystolic coeffs input = 
       take (length input) (simulate @System (goldenFIR coeffs (pure True)) input) 
    == take (length input) (drop 16 $ simulate @System (firSystolic (const (liftA3 macRealReal)) coeffs (pure True)) input)

prop_FilterSystolicSymmetric :: Vec 16 (Signed 32) -> [Signed 32] -> Bool
prop_FilterSystolicSymmetric coeffs input = 
       take (length input) (simulate @System (goldenFIR (coeffs Clash.++ Clash.reverse coeffs) (pure True)) input) 
    == take (length input) (drop 16 $ simulate @System (firSystolicSymmetric (const (liftA4 macPreAddRealReal)) coeffs (pure True)) input)

prop_FilterSymmetric :: Vec 16 (Signed 32) -> [Signed 32] -> Bool
prop_FilterSymmetric coeffs input = 
       take (length input) (simulate @System (Clash.register 0 . goldenFIR (Clash.reverse coeffs Clash.++ coeffs) (pure True)) input) 
    == take (length input) (simulate @System (firSymmetric (const (liftA4 macPreAddRealReal)) coeffs (pure True)) input)

prop_systolicSymmetric :: Vec 16 (Signed 32) -> Signed 32 -> [Signed 32] -> Bool
prop_systolicSymmetric coeffs mid input = 
       take (length input) (simulate @System (goldenFIR (coeffs Clash.++ Clash.singleton mid Clash.++ Clash.reverse coeffs) (pure True)) input)
    == take (length input) (drop 17 $ simulate @System (firSystolicSymmetricOdd (const (liftA4 macPreAddRealReal)) (coeffs Clash.++ Clash.singleton mid) (pure True)) input)

prop_systolicHalfBand :: Vec 16 (Signed 32) -> Signed 32 -> [Signed 32] -> Bool
prop_systolicHalfBand coeffs mid input = 
       take (length input) (simulate @System (goldenFIR (coeffs' Clash.++ Clash.singleton mid Clash.++ Clash.reverse coeffs') (pure True)) input)
    == take (length input) (drop 17 $ simulate @System (firSystolicHalfBand (const (liftA4 macPreAddRealReal)) (coeffs Clash.++ Clash.singleton mid) (pure True)) input)
    where
    coeffs' = Clash.init (Clash.merge coeffs (Clash.repeat 0))

streamList 
    :: (HiddenClockResetEnable dom, NFDataX a)
    => [a]
    -> [Bool]
    -> Signal dom Bool
    -> (Signal dom Bool, Signal dom a)
streamList samples enables = unbundle . mealy step (samples, enables)
    where
    step :: ([a], [Bool]) -> Bool -> (([a], [Bool]), (Bool, a))
    step (l@(x:xs), (e:es))     False = ((l, es),  (e, x))
    step (l@(x:xs), (False:es)) True  = ((l, es),  (False, x))
    step (  (x:xs), (True:es))  True  = ((xs, es), (True, x))

type Filter dom a
    =  Signal dom Bool                                  -- ^ Input valid
    -> Signal dom a                                     -- ^ Sample
    -> (Signal dom Bool, Signal dom a, Signal dom Bool) -- ^ (Output valid, output data, ready)

system 
    :: forall dom a
    .  (HiddenClockResetEnable dom, NFDataX a, Num a)
    => Filter dom a
    -> [a]
    -> [Bool]
    -> (Signal dom Bool, Signal dom a)
system filter samples ens = (vld, out)
    where
    (valids, sampleStream) = streamList samples (cycle ens) ready
    (vld, out, ready)      = filter valids sampleStream

prop_semiParallelFIRSystolic :: Vec 4 (Signed 32) -> [Signed 32] -> Property
prop_semiParallelFIRSystolic coeffs input = expect === result 
    where
    expect
        = take (length input) 
        $ sample @System 
        $ goldenFIR coeffs (pure True) (fromList $ 0 : input ++ repeat 0)
    result
        = take (length input) 
        $ map snd
        $ filter fst
        $ sample @System 
        $ bundle 
        $ system (semiParallelFIRSystolic (const macRealReal) (singleton coeffs)) (input ++ repeat 0) (cycle [False, True])

prop_semiParallelFIRSystolicMultiStage :: Vec 4 (Vec 4 (Signed 32)) -> [Signed 32] -> Property
prop_semiParallelFIRSystolicMultiStage coeffs input = expect === result 
    where
    expect
        = take (length input) 
        $ sample @System 
        $ goldenFIR (Clash.concat coeffs) (pure True) (fromList $ 0 : input ++ repeat 0)
    result
        = take (length input) 
        $ drop 3
        $ map snd
        $ filter fst
        $ sample @System 
        $ bundle 
        $ system (semiParallelFIRSystolic (const macRealReal) coeffs) (input ++ repeat 0) (cycle [False, True])

--Semi-parallel FIR filter has lots of tests because it is confusing
prop_semiParallelFIR1 :: Vec 9 (Signed 32) -> [Signed 32] -> Bool
prop_semiParallelFIR1 coeffs input = res1 == res2
    where
    coeffs2 :: Vec 3 (Vec 3 (Signed 32))
    coeffs2 =  Clash.unconcatI coeffs
    input'  =  input ++ repeat 0
    res1    =  take 50 $ simulate @System (goldenFIR coeffs (pure True)) input'
    res2    =  take 50 $ map head $ S.chunksOf 3 $ drop 9 $ simulate @System (semiParallelFIR coeffs2 (pure True)) (concatMap ((++ [0, 0]) . pure) input' ++ repeat 0)

prop_semiParallelFIR2 :: Vec 15 (Signed 32) -> [Signed 32] -> Bool
prop_semiParallelFIR2 coeffs input = res1 == res2
    where
    coeffs2 :: Vec 5 (Vec 3 (Signed 32))
    coeffs2 =  Clash.unconcatI coeffs
    input'  =  input ++ repeat 0
    res1    =  take 50 $ simulate @System (goldenFIR coeffs (pure True)) input'
    res2    =  take 50 $ map head $ S.chunksOf 3 $ drop 11 $ simulate @System (semiParallelFIR coeffs2 (pure True)) (concatMap ((++ [0, 0]) . pure) input' ++ repeat 0)

prop_semiParallelFIR3 :: Vec 20 (Signed 32) -> [Signed 32] -> Bool
prop_semiParallelFIR3 coeffs input = res1 == res2
    where
    coeffs2 :: Vec 5 (Vec 4 (Signed 32))
    coeffs2 =  Clash.unconcatI coeffs
    input'  =  input ++ repeat 0
    res1    =  take 50 $ simulate @System (goldenFIR coeffs (pure True)) input'
    res2    =  take 50 $ map head $ S.chunksOf 4 $ drop 12 $ simulate @System (semiParallelFIR coeffs2 (pure True)) (concatMap ((++ [0, 0, 0]) . pure) input' ++ repeat 0)

