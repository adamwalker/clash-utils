module DSP.FIR.SemiParallelSpec where

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

spec = describe "FIR filters" $ do
    describe "Semi-parallel systolic" $ do
        specify "Semi-parallel"             $ property prop_semiParallelFIRSystolic
        specify "Semi-parallel multi stage" $ property prop_semiParallelFIRSystolicMultiStage
        specify "Semi-parallel mac delay"   $ property prop_semiParallelFIRSystolicMultiStageMacDelay
    describe "Semi-parallel systolic symmetric" $ do
        specify "Semi-parallel systolic odd"             $ property prop_semiParallelFIRSystolicSymmetric
        specify "Semi-parallel systolic multi stage odd" $ property prop_semiParallelFIRSystolicSymmetricMultiStage
        specify "Semi-parallel systolic mac delay odd"   $ property prop_semiParallelFIRSystolicSymmetricMacDelay
        specify "Semi-parallel systolic even"            $ property prop_semiParallelFIRSystolicSymmetricEven
    describe "Semi-parallel transposed" $ do
        specify "Semi-parallel 1"     $ property prop_semiParallelFIRTransposed
    describe "Semi-parallel transposed block ram" $ do
        specify "Semi-parallel 1"     $ property prop_semiParallelFIRTransposedBlockam

prop_semiParallelFIRSystolic :: Vec 4 (Signed 32) -> [Signed 32] -> InfiniteList Bool -> Property
prop_semiParallelFIRSystolic coeffs input (InfiniteList ens _) = expect === result 
    where
    expect
        = drop 4
        $ goldenExpect coeffs input
    result
        = drop 4
        $ take (length input) 
        $ map snd . filter fst
        $ sample @System 
        $ system (semiParallelFIRSystolic (const macRealReal) (SNat @0) (singleton coeffs) (pure 0)) input ens

prop_semiParallelFIRSystolicMultiStage :: Vec 4 (Vec 4 (Signed 32)) -> [Signed 32] -> InfiniteList Bool -> Property
prop_semiParallelFIRSystolicMultiStage coeffs input (InfiniteList ens _) = expect === result 
    where
    expect
        = drop 16
        $ goldenExpect (Clash.concat coeffs) input
    result
        = drop 16
        $ take (length input) 
        $ map snd . filter fst
        $ sample @System 
        $ system (semiParallelFIRSystolic (const macRealReal) (SNat @0) coeffs (pure 0)) input ens

prop_semiParallelFIRSystolicMultiStageMacDelay :: Vec 4 (Vec 4 (Signed 32)) -> [Signed 32] -> InfiniteList Bool -> Property
prop_semiParallelFIRSystolicMultiStageMacDelay coeffs input (InfiniteList ens _) = expect === result 
    where
    expect
        = drop 16
        $ goldenExpect (Clash.concat coeffs) input
    result
        = drop 16
        $ take (length input) 
        $ map snd . filter fst
        $ sample @System 
        $ system (semiParallelFIRSystolic macRealRealPipelined (SNat @1) coeffs (pure 0)) input ens

prop_semiParallelFIRSystolicSymmetric :: Vec 4 (Signed 32) -> [Signed 32] -> InfiniteList Bool -> Property
prop_semiParallelFIRSystolicSymmetric coeffs input (InfiniteList ens _) = expect === result 
    where
    expect
        = drop 9 
        $ goldenExpect (coeffs Clash.++ Clash.singleton 1 Clash.++ Clash.reverse coeffs) input
    result
        = drop 9
        $ take (length input) 
        $ map snd . filter fst
        $ sample @System 
        $ system (semiParallelFIRSystolicSymmetric (const macPreAddRealReal) (oddSymmAccum (SNat @0) id) (SNat @0) (singleton coeffs) (pure 0)) input ens

prop_semiParallelFIRSystolicSymmetricEven :: Vec 4 (Signed 32) -> Signed 32 -> [Signed 32] -> InfiniteList Bool -> Property
prop_semiParallelFIRSystolicSymmetricEven coeffs midCoeff input (InfiniteList ens _) = expect === result 
    where
    expect
        = drop 10 
        $ goldenExpect (coeffs Clash.++ Clash.singleton midCoeff Clash.++ Clash.singleton midCoeff Clash.++ Clash.reverse coeffs) input
    result
        = drop 10
        $ take (length input) 
        $ map snd . filter fst
        $ sample @System 
        $ system (semiParallelFIRSystolicSymmetric (const macPreAddRealReal) (evenSymmAccum (SNat @0) (\x y -> (x + y) * midCoeff)) (SNat @0) (singleton coeffs) (pure 0)) input ens

prop_semiParallelFIRSystolicSymmetricMultiStage :: Vec 4 (Vec 4 (Signed 32)) -> [Signed 32] -> InfiniteList Bool -> Property
prop_semiParallelFIRSystolicSymmetricMultiStage coeffs input (InfiniteList ens _) = expect === result 
    where
    expect
        = drop 33
        $ goldenExpect (Clash.concat coeffs Clash.++ Clash.singleton 1 Clash.++ Clash.reverse (Clash.concat coeffs)) input
    result
        = drop 33
        $ take (length input) 
        $ map snd . filter fst
        $ sample @System 
        $ system (semiParallelFIRSystolicSymmetric (const macPreAddRealReal) (oddSymmAccum (SNat @0) id) (SNat @0) coeffs (pure 0)) input ens

prop_semiParallelFIRSystolicSymmetricMacDelay :: Vec 4 (Vec 4 (Signed 32)) -> [Signed 32] -> InfiniteList Bool -> Property
prop_semiParallelFIRSystolicSymmetricMacDelay coeffs input (InfiniteList ens _) = expect === result 
    where
    expect
        = drop 33
        $ goldenExpect (Clash.concat coeffs Clash.++ Clash.singleton 1 Clash.++ Clash.reverse (Clash.concat coeffs)) input
    result
        = drop 33 
        $ take (length input) 
        $ map snd . filter fst
        $ sample @System 
        $ system (semiParallelFIRSystolicSymmetric macPreAddRealRealPipelined (oddSymmAccum (SNat @2) id) (SNat @2) coeffs (pure 0)) input ens

prop_semiParallelFIRTransposed :: Vec 4 (Vec 3 (Signed 32)) -> [Signed 32] -> InfiniteList Bool -> Property
prop_semiParallelFIRTransposed coeffs input (InfiniteList ens _) = expect === result
    where
    expect 
        = goldenExpect (swizzle coeffs) input
    result
        = take (length input) 
        $ map snd . filter fst
        $ sample @System 
        $ system (semiParallelFIRTransposed (const macRealReal) coeffs) (0 : input) ens
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
        $ system (semiParallelFIRTransposedBlockRam (const macRealReal) coeffs) (0 : input) ens
    swizzle = Clash.concat . Clash.transpose . Clash.reverse

