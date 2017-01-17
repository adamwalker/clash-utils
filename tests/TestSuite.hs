{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeOperators, NoImplicitPrelude, FlexibleContexts, TemplateHaskell #-}

import Test.QuickCheck
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Data.Digits
import qualified Data.Complex as C

import CLaSH.Prelude
import qualified Prelude
import qualified Data.List as Prelude

import CLaSH.BCD
import CLaSH.FIRFilter
import CLaSH.CORDIC
import CLaSH.Sort
import CLaSH.Divide

--BCD conversion testing
propBCDConversion = 
    forAll (choose (0, 9999)) $ \(x :: Int) -> 
        dropWhile (==0) (toList (toDec (fromIntegral x :: BitVector 16) :: Vec 4 BCDDigit)) == Prelude.map fromIntegral (digits 10 x)

--FIR filter testing
propFilterTransposed :: (KnownNat (n + 1), KnownNat n) => Vec (n + 1) (Signed 32) -> [Signed 32] -> Bool
propFilterTransposed coeffs input = 
       Prelude.take (Prelude.length input) (simulate (fir coeffs (pure True)) input) 
    == Prelude.take (Prelude.length input) (Prelude.drop 1 (simulate (firTransposed (reverse coeffs) (pure True)) input))

propFilterLinearPhase :: Vec 64 (Signed 32) -> [Signed 32] -> Bool
propFilterLinearPhase coeffs input = 
       Prelude.take (Prelude.length input) (simulate (register 0 . fir (reverse coeffs ++ coeffs) (pure True)) input) 
    == Prelude.take (Prelude.length input) (simulate (firLinearPhase coeffs (pure True)) input)

--CORDIC testing
approxEqual :: Double -> Double -> Bool
approxEqual x y = abs (x - y) < 0.0001

realPart :: Complex a -> a
realPart (x :+ _) = x

imagPart :: Complex a -> a
imagPart (_ :+ y) = y

consts :: Vec 100 (SFixed 32 32)
consts = $(v (Prelude.take 100 arctans))

propCORDICVectorMode = 
    forAll (choose (0, 500000000)) $ \(x :: Double) -> 
        forAll (choose (0, 500000000)) $ \(y :: Double) -> 
            let res     = doIt $ CordicState (fromRational (toRational x) :+ fromRational (toRational y)) 0
                cplxNum = x C.:+ y
            in     approxEqual (kValue 100 * realToFrac (realPart (cplx res))) (C.magnitude cplxNum)
                && approxEqual (realToFrac (arg res)) (C.phase cplxNum)
    where

    doIt :: CordicState (SFixed 32 32) (SFixed 32 32) -> CordicState (SFixed 32 32) (SFixed 32 32)
    doIt = cordicSteps (\(CordicState (_ :+ y) _) -> y < 0) consts

propCORDICRotationMode = 
    forAll (choose (0, 5000)) $ \(x :: Double) -> 
        forAll (choose (0, 0.75)) $ \(y :: Double) -> 
            let res     = doIt $ CordicState (fromRational (toRational x) :+ 0) (fromRational (toRational y))
            in     approxEqual (kValue 100 * realToFrac (realPart (cplx res))) (x * Prelude.cos y)
                && approxEqual (kValue 100 * realToFrac (imagPart (cplx res))) (x * Prelude.sin y)
    where

    doIt :: CordicState (SFixed 32 32) (SFixed 32 32) -> CordicState (SFixed 32 32) (SFixed 32 32)
    doIt = cordicSteps (\(CordicState _ a) -> a > 0) consts

--Bitonic sorting network
propBitonicSort :: Vec 16 (Signed 32) -> Bool
propBitonicSort vec = toList (bitonicSorterExample vec) == Prelude.reverse (Prelude.sort (toList vec))

--Divider
propDivider :: BitVector 32 -> BitVector 32 -> Bool
propDivider x y = q == q' && r == r'
    where
    (q, r)   = quotRem x y
    (q', r') = combDivide x y

tests = [
        testProperty "BCD conversion"          propBCDConversion,
        testProperty "Transposed FIR filter"   (propFilterTransposed :: Vec 8 (Signed 32) -> [Signed 32] -> Bool),
        testProperty "Linear phase FIR filter" propFilterLinearPhase,
        testProperty "CORDIC vector mode"      propCORDICVectorMode,
        testProperty "CORDIC rotation mode"    propCORDICRotationMode,
        testProperty "Bitonic sorter"          propBitonicSort,
        testProperty "Divider"                 propDivider
    ]

main = defaultMain tests

