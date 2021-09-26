module ComplexSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable, type (+), extend, fromList, sample, System)
import Test.Hspec
import Test.QuickCheck hiding (sample)

import Clash.DSP.Complex

spec = describe "Complex numbers" $ do
    specify "Three multipler complex multiplication gives the correct result" $ property prop_complex3Mul

prop_complex3Mul :: Complex (Signed 16) -> Complex (Signed 16) -> Property
prop_complex3Mul x y = expect === (Clash.truncateB <$> result)
    where
    expect :: Complex (Signed 33)
    expect =  cMul x y 
    result :: Complex (Signed 34)
    result =  cMul3 x y
