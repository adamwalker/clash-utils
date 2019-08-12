module IIRFilterSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable, System)
import Test.Hspec
import Test.QuickCheck

import Clash.DSP.IIRFilter

--IIR filter testing
--Check that both direct forms are equivalent
spec = describe "IIR filters" $ do
    specify "direct"       $ property prop_IIRDirect
    specify "transposedI"  $ property prop_IIRTransposedI
    specify "transposedII" $ property prop_IIRTransposedII

prop_IIRDirect :: Vec 65 (Signed 32) -> Vec 64 (Signed 32) -> [Signed 32] -> Bool
prop_IIRDirect coeffs1 coeffs2 input = 
       take (length input) (simulate @System (iirDirectI coeffs1 coeffs2 (pure True)) input) 
    == take (length input) (simulate @System (iirDirectII coeffs1 coeffs2 (pure True)) input)

prop_IIRTransposedI :: Vec 65 (Signed 32) -> Vec 64 (Signed 32) -> [Signed 32] -> Bool
prop_IIRTransposedI coeffs1 coeffs2 input = 
       take (length input) (simulate @System (iirDirectI     coeffs1 coeffs2 (pure True)) input) 
    == take (length input) (simulate @System (iirTransposedI coeffs1 coeffs2 (pure True)) input)

prop_IIRTransposedII :: Vec 65 (Signed 32) -> Vec 64 (Signed 32) -> [Signed 32] -> Bool
prop_IIRTransposedII coeffs1 coeffs2 input = 
       take (length input) (simulate @System (iirDirectI     coeffs1 coeffs2 (pure True)) input) 
    == take (length input) (simulate @System (iirTransposedII coeffs1 coeffs2 (pure True)) input)

