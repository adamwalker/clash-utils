module LFSRSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable, System)
import Test.Hspec
import Test.QuickCheck

import Clash.LFSR
import Clash.Misc

--serial FFT
spec = describe "LFSRs" $ do
    specify "Fibonacci produces same sequence as Galois" $ property prop_fibEqualsGalois

prop_fibEqualsGalois :: BitVector 15 -> BitVector 16 -> Property
prop_fibEqualsGalois poly state = fib === gal
    where
    gal, fib :: Vec 64 Bit
    gal = fst $ galoisLFSRSteps poly (unpack state) 
    fib = fst $ fibonacciLFSRSteps (revBV poly) galAdvanced

    galAdvanced :: Vec 16 Bit
    galAdvanced = fst $ galoisLFSRSteps poly (unpack state)
