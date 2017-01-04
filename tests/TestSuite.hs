{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeOperators, NoImplicitPrelude, FlexibleContexts #-}

import Test.QuickCheck
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Data.Digits

import CLaSH.Prelude
import qualified Prelude

import CLaSH.BCD
import CLaSH.FIRFilter

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

tests = [
        testProperty "bcd conversion"          propBCDConversion,
        testProperty "Transposed FIR filter"   (propFilterTransposed :: Vec 8 (Signed 32) -> [Signed 32] -> Bool),
        testProperty "Linear phase FIR filter" propFilterLinearPhase
    ]

main = defaultMain tests
