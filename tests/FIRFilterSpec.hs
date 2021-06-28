module FIRFilterSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable, type (+), extend, NFDataX, System, fromList, singleton, sample)
import Control.Applicative
import Test.Hspec
import Test.QuickCheck hiding (sample)

import qualified Data.List.Split as S
import Clash.DSP.FIR.FIRFilter
import Clash.DSP.FIR.SemiParallel

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
    describe "Semi-parallel transposed" $ do
        specify "Semi-parallel 1"     $ property $ prop_semiParallelFIRTransposed
    describe "Semi-parallel transposed block ram" $ do
        specify "Semi-parallel 1"     $ property $ prop_semiParallelFIRTransposedBlockam

goldenFIR 
    :: (HiddenClockResetEnable dom, Num a, KnownNat n, NFDataX a)
    => Vec (n + 1) a
    -> Signal dom Bool
    -> Signal dom a
    -> Signal dom a
goldenFIR = fir (*) (+)

goldenExpect coeffs input
    = take (length input) 
    $ sample @System 
    $ goldenFIR coeffs (pure True) (fromList $ 0 : input ++ repeat 0) 

macRealReal c i a = c * i + a

macPreAddRealReal c x y a = c * (x + y) + a

liftA4 f w x y z = f <$> w <*> x <*> y <*> z

--A type for filters that can take an input and produce an output every cycle
type FilterNoReady dom a
    =  Signal dom Bool   -- ^ Input valid
    -> Signal dom a      -- ^ Sample
    -> Signal dom a      -- ^ (Output valid, output data)

systemNoReady
    :: forall dom a
    .  (HiddenClockResetEnable dom, NFDataX a, Num a)
    => FilterNoReady dom a
    -> [a]
    -> [Bool]
    -> (Signal dom Bool, Signal dom a)
systemNoReady filter samples ens = (valids, out)
    where
    (valids, sampleStream) = streamList samples ens (pure True)
    out                    = filter valids sampleStream

prop_FilterTransposed :: Vec 16 (Signed 32) -> [Signed 32] -> InfiniteList Bool -> Property
prop_FilterTransposed coeffs input (InfiniteList ens _) = expect === result
    where
    expect 
        = goldenExpect coeffs input
    result 
        = take (length input) 
        $ drop 2 
        $ map snd . filter fst
        $ sample @System 
        $ bundle
        $ systemNoReady (firTransposed (const (liftA3 macRealReal)) (Clash.reverse coeffs)) (0 : input ++ repeat 0) (True : ens)

prop_FilterSystolic :: Vec 16 (Signed 32) -> [Signed 32] -> InfiniteList Bool -> Property
prop_FilterSystolic coeffs input (InfiniteList ens _) = expect === result
    where
    expect 
        = goldenExpect coeffs input
    result 
        = take (length input) 
        $ drop 17 
        $ map snd . filter fst
        $ sample @System 
        $ bundle
        $ systemNoReady (firSystolic (const (liftA3 macRealReal)) coeffs) (0 : input ++ repeat 0) (True : ens)

prop_FilterSystolicSymmetric :: Vec 16 (Signed 32) -> [Signed 32] -> InfiniteList Bool -> Property
prop_FilterSystolicSymmetric coeffs input (InfiniteList ens _)= expect === result
    where
    expect 
        = goldenExpect (coeffs Clash.++ Clash.reverse coeffs) input
    result
        = take (length input) 
        $ drop 17 
        $ map snd . filter fst
        $ sample @System 
        $ bundle
        $ systemNoReady (firSystolicSymmetric (const (liftA4 macPreAddRealReal)) coeffs) (0 : input ++ repeat 0) (True : ens)

prop_FilterSymmetric :: Vec 16 (Signed 32) -> [Signed 32] -> InfiniteList Bool -> Property
prop_FilterSymmetric coeffs input (InfiniteList ens _) = expect === result
    where
    expect 
        = goldenExpect (Clash.reverse coeffs Clash.++ coeffs) input
    result
        = take (length input) 
        $ drop 2
        $ map snd . filter fst
        $ sample @System 
        $ bundle
        $ systemNoReady (firSymmetric (const (liftA4 macPreAddRealReal)) coeffs) (0 : input ++ repeat 0) (True : ens)

prop_systolicSymmetric :: Vec 16 (Signed 32) -> Signed 32 -> [Signed 32] -> InfiniteList Bool -> Property
prop_systolicSymmetric coeffs mid input (InfiniteList ens _) = expect === result
    where
    expect 
        = goldenExpect (coeffs Clash.++ Clash.singleton mid Clash.++ Clash.reverse coeffs) input
    result
        = take (length input) 
        $ drop 18 
        $ map snd . filter fst
        $ sample @System 
        $ bundle
        $ systemNoReady (firSystolicSymmetricOdd (const (liftA4 macPreAddRealReal)) (coeffs Clash.++ Clash.singleton mid)) (0 : input ++ repeat 0) (True : ens)

prop_systolicHalfBand :: Vec 16 (Signed 32) -> Signed 32 -> [Signed 32] -> InfiniteList Bool -> Property
prop_systolicHalfBand coeffs mid input (InfiniteList ens _) = expect === result
    where
    expect 
        = goldenExpect (coeffs' Clash.++ Clash.singleton mid Clash.++ Clash.reverse coeffs') input
    result 
        = take (length input) 
        $ drop 18 
        $ map snd . filter fst
        $ sample @System 
        $ bundle
        $ systemNoReady (firSystolicHalfBand (const (liftA4 macPreAddRealReal)) (coeffs Clash.++ Clash.singleton mid)) (0 : input ++ repeat 0) (True : ens)
    coeffs' 
        = Clash.init (Clash.merge coeffs (Clash.repeat 0))

streamList 
    :: (HiddenClockResetEnable dom, NFDataX a)
    => [a]
    -> [Bool]
    -> Signal dom Bool
    -> (Signal dom Bool, Signal dom a)
streamList samples enables = unbundle . mealy step (samples, enables)
    where
    step :: ([a], [Bool]) -> Bool -> (([a], [Bool]), (Bool, a))
    step (l@(x:xs), es@(True:_)) False = ((l, es),  (True, x))
    step (l@(x:xs), (e:es))      False = ((l, es),  (e, x))
    step (l@(x:xs), (False:es))  True  = ((l, es),  (False, x))
    step (  (x:xs), (True:es))   True  = ((xs, es), (True, x))

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
    (valids, sampleStream) = streamList samples ens ready
    (vld, out, ready)      = filter valids sampleStream

prop_semiParallelFIRSystolic :: Vec 4 (Signed 32) -> [Signed 32] -> InfiniteList Bool -> Property
prop_semiParallelFIRSystolic coeffs input (InfiniteList ens _) = expect === result 
    where
    expect
        = goldenExpect coeffs input
    result
        = take (length input) 
        $ map snd . filter fst
        $ sample @System 
        $ bundle 
        $ system (semiParallelFIRSystolic (const macRealReal) (singleton coeffs)) (input ++ repeat 0) ens

prop_semiParallelFIRSystolicMultiStage :: Vec 4 (Vec 4 (Signed 32)) -> [Signed 32] -> InfiniteList Bool -> Property
prop_semiParallelFIRSystolicMultiStage coeffs input (InfiniteList ens _) = expect === result 
    where
    expect
        = goldenExpect (Clash.concat coeffs) input
    result
        = take (length input) 
        $ map snd . filter fst
        $ sample @System 
        $ bundle 
        $ system (semiParallelFIRSystolic (const macRealReal) coeffs) (input ++ repeat 0) ens

prop_semiParallelFIRTransposed :: Vec 4 (Vec 3 (Signed 32)) -> [Signed 32] -> InfiniteList Bool -> Property
prop_semiParallelFIRTransposed coeffs input (InfiniteList ens _) = expect === result
    where
    expect 
        = goldenExpect (swizzle coeffs) input
    result
        = take (length input) 
        $ map snd . filter fst
        $ sample @System 
        $ bundle 
        $ system (semiParallelFIRTransposed (const macRealReal) coeffs) (0 : input ++ repeat 0) ens
    swizzle = Clash.concat . Clash.transpose . Clash.reverse

prop_semiParallelFIRTransposedBlockam :: Vec 2 (Vec 3 (Signed 32)) -> [Signed 32] -> InfiniteList Bool -> Property
prop_semiParallelFIRTransposedBlockam coeffs input (InfiniteList ens _) = expect === result
    where
    expect 
        = take (length input) 
        $ drop 5 --Drop the Xs since the asyncRam doesnt support initial values
        $ sample @System 
        $ goldenFIR (swizzle coeffs) (pure True) (fromList $ 0 : input ++ repeat 0)
    result
        = take (length input) 
        $ drop 6 --Drop the Xs since the asyncRam doesnt support initial values
        $ map snd . filter fst
        $ sample @System 
        $ bundle 
        $ system (semiParallelFIRTransposedBlockRam (const macRealReal) coeffs) (0 : input ++ repeat 0) ens
    swizzle = Clash.concat . Clash.transpose . Clash.reverse

