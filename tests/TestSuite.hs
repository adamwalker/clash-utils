{-# LANGUAGE DataKinds, ScopedTypeVariables #-}

import Test.QuickCheck
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Digits
import CLaSH.BCD
import CLaSH.Prelude

tests = [
        testProperty "bcd conversion" propBCDConversion
    ]

propBCDConversion = 
    forAll (choose (0, 9999)) $ \(x :: Int) -> 
        dropWhile (==0) (toList (toDec (fromIntegral x :: BitVector 16) :: Vec 4 BCDDigit)) == Prelude.map fromIntegral (digits 10 x)

main = defaultMain tests
