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
import Clash.DSP.FIR.Polyphase

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
    describe "Polyphase" $ do
        specify "Decimate"               $ property $ prop_polyphaseDecim
        --specify "Decimate multi stage"   $ property $ prop_polyphaseDecimMultiStage
        specify "Decimate multi stage 2" $ property $ prop_polyphaseDecimMultiStage2
        specify "Decimate back pressure" $ property $ prop_polyphaseDecimMultiStage2_backPressure

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
    -> Signal dom (Bool, a)
systemNoReady filter samples ens = bundle (valids, out)
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
        $ drop 1 
        $ map snd . filter fst
        $ sample @System 
        $ systemNoReady (firTransposed (const (liftA3 macRealReal)) (Clash.reverse coeffs)) (input ++ repeat 0) (True : ens)

prop_FilterSystolic :: Vec 16 (Signed 32) -> [Signed 32] -> InfiniteList Bool -> Property
prop_FilterSystolic coeffs input (InfiniteList ens _) = expect === result
    where
    expect 
        = goldenExpect coeffs input
    result 
        = take (length input) 
        $ drop 16 
        $ map snd . filter fst
        $ sample @System 
        $ systemNoReady (firSystolic (const (liftA3 macRealReal)) coeffs) (input ++ repeat 0) (True : ens)

prop_FilterSystolicSymmetric :: Vec 16 (Signed 32) -> [Signed 32] -> InfiniteList Bool -> Property
prop_FilterSystolicSymmetric coeffs input (InfiniteList ens _)= expect === result
    where
    expect 
        = goldenExpect (coeffs Clash.++ Clash.reverse coeffs) input
    result
        = take (length input) 
        $ drop 16 
        $ map snd . filter fst
        $ sample @System 
        $ systemNoReady (firSystolicSymmetric (const (liftA4 macPreAddRealReal)) coeffs) (input ++ repeat 0) (True : ens)

prop_FilterSymmetric :: Vec 16 (Signed 32) -> [Signed 32] -> InfiniteList Bool -> Property
prop_FilterSymmetric coeffs input (InfiniteList ens _) = expect === result
    where
    expect 
        = goldenExpect (Clash.reverse coeffs Clash.++ coeffs) input
    result
        = take (length input) 
        $ drop 1
        $ map snd . filter fst
        $ sample @System 
        $ systemNoReady (firSymmetric (const (liftA4 macPreAddRealReal)) coeffs) (input ++ repeat 0) (True : ens)

prop_systolicSymmetric :: Vec 16 (Signed 32) -> Signed 32 -> [Signed 32] -> InfiniteList Bool -> Property
prop_systolicSymmetric coeffs mid input (InfiniteList ens _) = expect === result
    where
    expect 
        = goldenExpect (coeffs Clash.++ Clash.singleton mid Clash.++ Clash.reverse coeffs) input
    result
        = take (length input) 
        $ drop 17 
        $ map snd . filter fst
        $ sample @System 
        $ systemNoReady (firSystolicSymmetricOdd (const (liftA4 macPreAddRealReal)) (coeffs Clash.++ Clash.singleton mid)) (input ++ repeat 0) (True : ens)

prop_systolicHalfBand :: Vec 16 (Signed 32) -> Signed 32 -> [Signed 32] -> InfiniteList Bool -> Property
prop_systolicHalfBand coeffs mid input (InfiniteList ens _) = expect === result
    where
    expect 
        = goldenExpect (coeffs' Clash.++ Clash.singleton mid Clash.++ Clash.reverse coeffs') input
    result 
        = take (length input) 
        $ drop 17 
        $ map snd . filter fst
        $ sample @System 
        $ systemNoReady (firSystolicHalfBand (const (liftA4 macPreAddRealReal)) (coeffs Clash.++ Clash.singleton mid)) (input ++ repeat 0) (True : ens)
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

system 
    :: forall dom a
    .  (HiddenClockResetEnable dom, NFDataX a, Num a)
    => Filter dom a
    -> [a]
    -> [Bool]
    -> Signal dom (Bool, a)
system filter samples ens = bundle (vld, out)
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
        $ system (semiParallelFIRTransposedBlockRam (const macRealReal) coeffs) (0 : input ++ repeat 0) ens
    swizzle = Clash.concat . Clash.transpose . Clash.reverse

stride :: Int -> [a] -> [a]
stride s = go
    where
    go (x:xs) = x : go (Prelude.drop s xs)
    go _      = []

fst3 (x, _, _) = x
snd3 (_, y, _) = y

prop_polyphaseDecim :: Vec 2 (Vec 1 (Vec 2 (Signed 32))) -> [Signed 32] -> InfiniteList Bool -> Property
prop_polyphaseDecim coeffs input (InfiniteList ens _) = expect === result
    where
    expect
        = take (length input)
        $ stride 1 
        $ sample @System 
        $ goldenFIR (Clash.concat $ Clash.transpose $ Clash.map Clash.concat coeffs) (pure True) (fromList $ 0 : input ++ repeat 0)
    result 
        = take (length input) 
        $ map snd . filter fst
        $ sample @System 
        $ system (polyphaseDecim filters) (input ++ repeat 0) ens
    filters :: HiddenClockResetEnable dom => Vec 2 (Filter dom (Signed 32))
    filters = Clash.map (semiParallelFIRSystolic (const macRealReal)) $ Clash.reverse coeffs

prop_polyphaseDecimMultiStage :: Vec 2 (Vec 4 (Vec 2 (Signed 32))) -> [Signed 32] -> Property
prop_polyphaseDecimMultiStage coeffs input = expect === result
    where
    expect
        = Prelude.take (Prelude.length input)
        $ stride 1 
        $ sample @System 
        $ goldenFIR (Clash.concat $ Clash.transpose $ Clash.map Clash.concat coeffs) (pure True) (fromList $ 0 : input Prelude.++ Prelude.repeat 0)
    result 
        = Prelude.take (Prelude.length input) 
        $ Prelude.map snd3
        $ Prelude.drop 1
        $ Prelude.filter fst3
        $ sample @System 
        $ bundle 
        $ polyphaseDecim filters (pure True) (fromList $ 0 : input Prelude.++ Prelude.repeat 0) 
    filters :: HiddenClockResetEnable dom => Vec 2 (Filter dom (Signed 32))
    filters = Clash.map (semiParallelFIRSystolic (const macRealReal)) $ Clash.reverse coeffs

prop_polyphaseDecimMultiStage2 :: Vec 4 (Vec 4 (Vec 4 (Signed 32))) -> [Signed 32] -> InfiniteList Bool -> Property
prop_polyphaseDecimMultiStage2 coeffs input (InfiniteList ens _) = expect === result
    where
    expect
        = take (length input)
        $ drop 3
        $ stride 3 
        $ sample @System 
        $ goldenFIR (Clash.concat $ Clash.transpose $ Clash.map Clash.concat coeffs) (pure True) (fromList $ 0 : input ++ repeat 0)
    result 
        = take (length input) 
        $ map snd . drop 3 . filter fst
        $ sample @System 
        $ system (polyphaseDecim filters) (input ++ repeat 0) ens
    filters :: HiddenClockResetEnable dom => Vec 4 (Filter dom (Signed 32))
    filters = Clash.map (semiParallelFIRSystolic (const macRealReal)) $ Clash.reverse coeffs

--Phases contain long MAC groups so must assert backpressure
prop_polyphaseDecimMultiStage2_backPressure :: Vec 4 (Vec 4 (Vec 8 (Signed 32))) -> [Signed 32] -> InfiniteList Bool -> Property
prop_polyphaseDecimMultiStage2_backPressure coeffs input (InfiniteList ens _) = expect === result
    where
    expect
        = take (length input)
        $ drop 3
        $ stride 3 
        $ sample @System 
        $ goldenFIR (Clash.concat $ Clash.transpose $ Clash.map Clash.concat coeffs) (pure True) (fromList $ 0 : input ++ repeat 0)
    result 
        = take (length input) 
        $ drop 3
        $ map snd
        $ filter fst
        $ sample @System 
        $ system (polyphaseDecim filters) (input ++ repeat 0) ens
    filters :: HiddenClockResetEnable dom => Vec 4 (Filter dom (Signed 32))
    filters = Clash.map (semiParallelFIRSystolic (const macRealReal)) $ Clash.reverse coeffs

