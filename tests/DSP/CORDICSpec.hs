module DSP.CORDICSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable, sample, System)
import Test.Hspec
import Test.QuickCheck hiding (sample)

import qualified Data.Complex as C
import Clash.DSP.CORDIC

approxEqual :: Double -> Double -> Property
approxEqual x y = counterexample cex $ abs (x - y) < 0.0001
    where
    cex = "Got: " ++ show x ++ ", expected " ++ show y

--CORDIC testing
spec = describe "CORDIC" $ do
    specify "vector mode"                          $ property prop_CORDICVectorMode
    specify "vector mode binary angle measurement" $ property prop_CORDICVectorModeBAM
    specify "rotation mode"                        $ property prop_CORDICRotationMode
    specify "pipelined"                            $ property prop_CORDICPipelined
    specify "toPolar"                              $ property prop_toPolar

consts :: Vec 100 (SFixed 2 32)
consts = $(listToVecTH (Prelude.take 100 arctans))

constsBAM :: Vec 100 (SFixed 1 32)
constsBAM = $(listToVecTH (Prelude.take 100 $ Prelude.map (/pi) arctans))

--Test CORDIC vector mode by calculating the magnitude and phase of a complex number
prop_CORDICVectorMode = 
    forAll (choose (0, 500000000)) $ \(x :: Double) -> --Restrict the input ranges so that the error does not grow beyond what approxEqual considers acceptable
        forAll (choose (-500000000, 500000000)) $ \(y :: Double) -> 
            let res     = doIt $ CordicState (fromRational (toRational x) :+ fromRational (toRational y)) 0
                cplxNum = x C.:+ y
            in       approxEqual (kValue 100 * realToFrac (realPart (cplx res))) (C.magnitude cplxNum)
                .&&. approxEqual (realToFrac (arg res)) (C.phase cplxNum)
    where

    doIt :: CordicState (SFixed 32 32) (SFixed 2 32) -> CordicState (SFixed 32 32) (SFixed 2 32)
    doIt = cordicSteps dirMagPhase (0 :: Index 100) consts

--Test CORDIC vector mode by calculating the magnitude and phase of a complex number
prop_CORDICVectorModeBAM = 
    forAll (choose (0, 500000000)) $ \(x :: Double) -> --Restrict the input ranges so that the error does not grow beyond what approxEqual considers acceptable
        forAll (choose (-500000000, 500000000)) $ \(y :: Double) -> 
            let res     = doIt $ CordicState (fromRational (toRational x) :+ fromRational (toRational y)) 0
                cplxNum = x C.:+ y
            in       approxEqual (kValue 100 * realToFrac (realPart (cplx res))) (C.magnitude cplxNum)
                .&&. approxEqual (realToFrac (arg res)) (C.phase cplxNum / pi)
    where

    doIt :: CordicState (SFixed 32 32) (SFixed 1 32) -> CordicState (SFixed 32 32) (SFixed 1 32)
    doIt = cordicSteps dirMagPhase (0 :: Index 100) constsBAM

--Test CORDIC rotation mode by calculating the real and imaginary part of a complex number given in polar form
prop_CORDICRotationMode = 
    forAll (choose (0, 5000)) $ \(x :: Double) ->  --Restrict the input ranges so that the error does not grow beyond what approxEqual considers acceptable
        forAll (choose (0, 0.75)) $ \(y :: Double) -> 
            let res     = doIt $ CordicState (fromRational (toRational x) :+ 0) (fromRational (toRational y))
            in       approxEqual (kValue 100 * realToFrac (realPart (cplx res))) (x * Prelude.cos y)
                .&&. approxEqual (kValue 100 * realToFrac (imagPart (cplx res))) (x * Prelude.sin y)
    where

    doIt :: CordicState (SFixed 32 32) (SFixed 2 32) -> CordicState (SFixed 32 32) (SFixed 2 32)
    doIt = cordicSteps dirRealImag (0 :: Index 100) consts

--Test CORDIC vector mode by calculating the magnitude and phase of a complex number
prop_CORDICPipelined = 
    forAll (choose (0, 500000000)) $ \(x :: Double) -> --Restrict the input ranges so that the error does not grow beyond what approxEqual considers acceptable
        forAll (choose (-500000000, 500000000)) $ \(y :: Double) -> 
            let res     = doIt $ CordicState (fromRational (toRational x) :+ fromRational (toRational y)) 0
                cplxNum = x C.:+ y
            in       approxEqual (kValue 100 * realToFrac (realPart (cplx res))) (C.magnitude cplxNum)
                .&&. approxEqual (realToFrac (arg res)) (C.phase cplxNum)
    where

    doIt :: CordicState (SFixed 32 32) (SFixed 2 32) -> CordicState (SFixed 32 32) (SFixed 2 32)
    doIt input 
        = head
        $ drop 10
        $ sample @System 
        $ cordicPipeline dirMagPhase (0 :: Index 100) (Clash.unconcat (SNat @10) consts) (pure True)
        $ pure input

--Test CORDIC vector mode by calculating the magnitude and phase of a complex number
prop_toPolar = 
    forAll (choose (-500000000, 500000000)) $ \(x :: Double) -> --Restrict the input ranges so that the error does not grow beyond what approxEqual considers acceptable
        forAll (choose (-500000000, 500000000)) $ \(y :: Double) -> 
            let res     = doIt $ fromRational (toRational x) :+ fromRational (toRational y)
                cplxNum = x C.:+ y
            in       approxEqual (kValue 100 * realToFrac (fst res)) (C.magnitude cplxNum)
                .&&. approxEqual (realToFrac (snd res)) (C.phase cplxNum / pi)
    where

    doIt :: Complex (SFixed 32 32) -> (SFixed 32 32, SFixed 1 32)
    doIt input 
        = head
        $ drop 11
        $ sample @System 
        $ toPolar (Clash.unconcat (SNat @10) constsBAM) (pure True) (pure input)

