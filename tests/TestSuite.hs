{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeOperators, NoImplicitPrelude, FlexibleContexts, TemplateHaskell #-}

import Test.QuickCheck
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Data.Digits
import qualified Data.Complex as C

import CLaSH.Prelude
import qualified Prelude

import CLaSH.BCD
import CLaSH.FIRFilter
import CLaSH.CORDIC

propBCDConversion = 
    forAll (choose (0, 9999)) $ \(x :: Int) -> 
        dropWhile (==0) (toList (toDec (fromIntegral x :: BitVector 16) :: Vec 4 BCDDigit)) == Prelude.map fromIntegral (digits 10 x)

propFilterTransposed :: (KnownNat (n + 1), KnownNat n) => Vec (n + 1) (Signed 32) -> [Signed 32] -> Bool
propFilterTransposed coeffs input = 
       Prelude.take (Prelude.length input) (simulate (fir coeffs) input) 
    == Prelude.take (Prelude.length input) (Prelude.drop 1 (simulate (firTransposed (reverse coeffs)) input))

propFilterLinearPhase :: Vec 64 (Signed 32) -> [Signed 32] -> Bool
propFilterLinearPhase coeffs input = 
       Prelude.take (Prelude.length input) (simulate (register 0 . fir (reverse coeffs ++ coeffs)) input) 
    == Prelude.take (Prelude.length input) (simulate (firLinearPhase coeffs) input)

propCORDICVectorMode = 
    forAll (choose (0, 500000000)) $ \(x :: Int) -> 
        forAll (choose (0, 500000000)) $ \(y :: Int) -> 
            let res     = doIt $ CordicState (fromIntegral x :+ fromIntegral y) 0
                cplxNum = fromIntegral x C.:+ fromIntegral y
            in     approxEqual (kValue 100 * realToFrac (realPart (cplx res))) (C.magnitude cplxNum)
                && approxEqual (realToFrac (arg res)) (C.phase cplxNum)
    where
    realPart :: Complex a -> a
    realPart (x :+ y) = x

    consts :: Vec 100 (SFixed 32 32)
    consts = $(v (Prelude.take 100 tangents))

    doIt :: CordicState (SFixed 32 32) (SFixed 32 32) -> CordicState (SFixed 32 32) (SFixed 32 32)
    doIt = cordic (\(CordicState (_ :+ y) _) -> y < 0) consts

    approxEqual :: Double -> Double -> Bool
    approxEqual x y = abs (x - y) < 0.0001

tests = [
        testProperty "bcd conversion"          propBCDConversion,
        testProperty "Transposed FIR filter"   (propFilterTransposed :: Vec 8 (Signed 32) -> [Signed 32] -> Bool),
        testProperty "Linear phase FIR filter" propFilterLinearPhase,
        testProperty "CORDIC vector mode"      propCORDICVectorMode
    ]

main = defaultMain tests
