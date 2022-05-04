module DSP.FIR.ParallelSpec where

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
    describe "Parallel" $ do
        specify "transposed"          $ property prop_FilterTransposed
        specify "systolic"            $ property prop_FilterSystolic
        specify "systolic symmetric"  $ property prop_FilterSystolicSymmetric
        specify "symmetric"           $ property prop_FilterSymmetric
        specify "systolic symmetric"  $ property prop_systolicSymmetric
        specify "systolic half band"  $ property prop_systolicHalfBand

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
        $ systemNoReady (firTransposed (const (liftA3 macRealReal)) (Clash.reverse coeffs)) input (True : ens)

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
        $ systemNoReady (firSystolic (const (liftA3 macRealReal)) coeffs) input (True : ens)

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
        $ systemNoReady (firSystolicSymmetric (const (liftA4 macPreAddRealReal)) coeffs) input (True : ens)

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
        $ systemNoReady (firSymmetric (const (liftA4 macPreAddRealReal)) coeffs) input (True : ens)

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
        $ systemNoReady (firSystolicSymmetricOdd (const (liftA4 macPreAddRealReal)) (coeffs Clash.++ Clash.singleton mid)) input (True : ens)

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
        $ systemNoReady (firSystolicHalfBand (const (liftA4 macPreAddRealReal)) (coeffs Clash.++ Clash.singleton mid)) input (True : ens)
    coeffs' 
        = Clash.init (Clash.merge coeffs (Clash.repeat 0))

