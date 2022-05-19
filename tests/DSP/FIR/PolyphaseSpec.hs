module DSP.FIR.PolyphaseSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable, type (+), extend, NFDataX, System, fromList, singleton, sample, regEn)
import Control.Applicative
import Test.Hspec
import Test.QuickCheck hiding (sample)

import qualified Data.List.Split as S
import Clash.DSP.FIR.Filter
import Clash.DSP.FIR.SemiParallel
import Clash.DSP.FIR.Polyphase

import DSP.FIR.Common

--FIR filter testing
--Check that both optimised filters return the same results as the unoptimised one
spec = describe "FIR filters" $ do
    describe "Polyphase" $ do
        specify "Decimate"                 $ property prop_polyphaseDecim
        specify "Decimate multi stage"     $ property prop_polyphaseDecimMultiStage
        specify "Decimate multi stage 2"   $ property prop_polyphaseDecimMultiStage2
        specify "Decimate multi stage 3"   $ property prop_polyphaseDecimMultiStage3
        specify "Decimate multi stage 4"   $ property prop_polyphaseDecimMultiStage4
        specify "Decimate back pressure"   $ property prop_polyphaseDecimMultiStage2_backPressure
        specify "Decimate back pressure 2" $ property prop_polyphaseDecimMultiStage3_backPressure

--2 phases
--1 stage in semi parallel filter
--2 coefficients in filter
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
        $ system (polyphaseDecim filters) input ens
    filters :: HiddenClockResetEnable dom => Vec 2 (Filter dom (Signed 32))
    filters = Clash.map (\coeffs -> semiParallelFIRSystolic (const macRealReal) (SNat @0) coeffs (pure 0)) $ Clash.reverse coeffs

--2 phases
--2 stages in semi parallel filter
--2 coefficients in filter
prop_polyphaseDecimMultiStage :: Vec 2 (Vec 2 (Vec 2 (Signed 32))) -> [Signed 32] -> InfiniteList Bool -> Property
prop_polyphaseDecimMultiStage coeffs input (InfiniteList ens _) = expect === result
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
        $ system (polyphaseDecim filters) input ens
    filters :: HiddenClockResetEnable dom => Vec 2 (Filter dom (Signed 32))
    filters = Clash.map (\coeffs -> semiParallelFIRSystolic (const macRealReal) (SNat @0) coeffs (pure 0)) $ Clash.reverse coeffs

--2 phases
--4 stages in semi parallel filter
--2 coefficients in filter
prop_polyphaseDecimMultiStage2 :: Vec 2 (Vec 4 (Vec 2 (Signed 32))) -> [Signed 32] -> InfiniteList Bool -> Property
prop_polyphaseDecimMultiStage2 coeffs input (InfiniteList ens _) = expect === result
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
        $ system (polyphaseDecim filters) input ens
    filters :: HiddenClockResetEnable dom => Vec 2 (Filter dom (Signed 32))
    filters = Clash.map (\coeffs -> semiParallelFIRSystolic (const macRealReal) (SNat @0) coeffs (pure 0)) $ Clash.reverse coeffs

--4 phases
--4 stages in semi parallel filter
--4 coefficients in filter
prop_polyphaseDecimMultiStage3 :: Vec 4 (Vec 4 (Vec 4 (Signed 32))) -> [Signed 32] -> InfiniteList Bool -> Property
prop_polyphaseDecimMultiStage3 coeffs input (InfiniteList ens _) = expect === result
    where
    expect
        = take (length input)
        $ stride 3 
        $ sample @System 
        $ goldenFIR (Clash.concat $ Clash.transpose $ Clash.map Clash.concat coeffs) (pure True) (fromList $ 0 : input ++ repeat 0)
    result 
        = take (length input) 
        $ map snd . filter fst
        $ sample @System 
        $ system (polyphaseDecim filters) input ens
    filters :: HiddenClockResetEnable dom => Vec 4 (Filter dom (Signed 32))
    filters = Clash.map (\coeffs -> semiParallelFIRSystolic (const macRealReal) (SNat @0) coeffs (pure 0)) $ Clash.reverse coeffs

--3 phases
--2 stages in semi parallel filter
--3 coefficients in filter
prop_polyphaseDecimMultiStage4 :: Vec 3 (Vec 2 (Vec 3 (Signed 32))) -> [Signed 32] -> InfiniteList Bool -> Property
prop_polyphaseDecimMultiStage4 coeffs input (InfiniteList ens _) = expect === result
    where
    expect
        = take (length input)
        $ stride 2 
        $ sample @System 
        $ goldenFIR (Clash.concat $ Clash.transpose $ Clash.map Clash.concat coeffs) (pure True) (fromList $ 0 : input ++ repeat 0)
    result 
        = take (length input) 
        $ map snd . filter fst
        $ sample @System 
        $ system (polyphaseDecim filters) input ens
    filters :: HiddenClockResetEnable dom => Vec 3 (Filter dom (Signed 32))
    filters = Clash.map (\coeffs -> semiParallelFIRSystolic (const macRealReal) (SNat @0) coeffs (pure 0)) $ Clash.reverse coeffs

--Phases contain long MAC groups so must assert backpressure
--4 phases
--4 stages in semi parallel filter
--8 coefficients in filter
prop_polyphaseDecimMultiStage2_backPressure :: Vec 4 (Vec 4 (Vec 8 (Signed 32))) -> [Signed 32] -> InfiniteList Bool -> Property
prop_polyphaseDecimMultiStage2_backPressure coeffs input (InfiniteList ens _) = expect === result
    where
    expect
        = take (length input)
        $ stride 3 
        $ sample @System 
        $ goldenFIR (Clash.concat $ Clash.transpose $ Clash.map Clash.concat coeffs) (pure True) (fromList $ 0 : input ++ repeat 0)
    result 
        = take (length input) 
        $ map snd . filter fst
        $ sample @System 
        $ system (polyphaseDecim filters) input ens
    filters :: HiddenClockResetEnable dom => Vec 4 (Filter dom (Signed 32))
    filters = Clash.map (\coeffs -> semiParallelFIRSystolic (const macRealReal) (SNat @0) coeffs (pure 0)) $ Clash.reverse coeffs

--Phases contain long MAC groups so must assert backpressure
--2 phases
--4 stages in semi parallel filter
--8 coefficients in filter
prop_polyphaseDecimMultiStage3_backPressure :: Vec 2 (Vec 8 (Vec 8 (Signed 32))) -> [Signed 32] -> InfiniteList Bool -> Property
prop_polyphaseDecimMultiStage3_backPressure coeffs input (InfiniteList ens _) = expect === result
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
        $ system (polyphaseDecim filters) input ens
    filters :: HiddenClockResetEnable dom => Vec 2 (Filter dom (Signed 32))
    filters = Clash.map (\coeffs -> semiParallelFIRSystolic (const macRealReal) (SNat @0) coeffs (pure 0)) $ Clash.reverse coeffs

