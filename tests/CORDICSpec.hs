module CORDICSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockReset)
import Test.Hspec
import Test.QuickCheck

import qualified Data.Complex as C
import Clash.CORDIC

approxEqual :: Double -> Double -> Bool
approxEqual x y = abs (x - y) < 0.0001

--CORDIC testing
spec = describe "CORDIC" $ do
    specify "vector mode"    $ property prop_CORDICVectorMode
    specify "rotation mode " $ property prop_CORDICRotationMode

consts :: Vec 100 (SFixed 32 32)
consts = $(listToVecTH (Prelude.take 100 arctans))

--Test CORDIC vector mode by calculating the magnitude and phase of a complex number
prop_CORDICVectorMode = 
    forAll (choose (0, 500000000)) $ \(x :: Double) -> --Restrict the input ranges so that the error does not grow beyond what approxEqual considers acceptable
        forAll (choose (0, 500000000)) $ \(y :: Double) -> 
            let res     = doIt $ CordicState (fromRational (toRational x) :+ fromRational (toRational y)) 0
                cplxNum = x C.:+ y
            in     approxEqual (kValue 100 * realToFrac (realPart (cplx res))) (C.magnitude cplxNum)
                && approxEqual (realToFrac (arg res)) (C.phase cplxNum)
    where

    doIt :: CordicState (SFixed 32 32) (SFixed 32 32) -> CordicState (SFixed 32 32) (SFixed 32 32)
    doIt = cordicSteps (\(CordicState (_ :+ y) _) -> y < 0) consts

--Test CORDIC rotation mode by calculating the real and imaginary part of a complex number given in polar form
prop_CORDICRotationMode = 
    forAll (choose (0, 5000)) $ \(x :: Double) ->  --Restrict the input ranges so that the error does not grow beyond what approxEqual considers acceptable
        forAll (choose (0, 0.75)) $ \(y :: Double) -> 
            let res     = doIt $ CordicState (fromRational (toRational x) :+ 0) (fromRational (toRational y))
            in     approxEqual (kValue 100 * realToFrac (realPart (cplx res))) (x * Prelude.cos y)
                && approxEqual (kValue 100 * realToFrac (imagPart (cplx res))) (x * Prelude.sin y)
    where

    doIt :: CordicState (SFixed 32 32) (SFixed 32 32) -> CordicState (SFixed 32 32) (SFixed 32 32)
    doIt = cordicSteps (\(CordicState _ a) -> a > 0) consts
