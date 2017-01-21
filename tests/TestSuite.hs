{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeOperators, NoImplicitPrelude, FlexibleContexts, TemplateHaskell, BinaryLiterals #-}

import Control.Monad
import System.Exit

import Test.QuickCheck
import Test.QuickCheck.All

import Data.Digits
import qualified Data.Complex as C
import Data.Digest.CRC32

import CLaSH.Prelude
import qualified Prelude
import qualified Data.List as Prelude
import Data.Word

import CLaSH.BCD
import CLaSH.FIRFilter
import CLaSH.CORDIC
import CLaSH.Sort
import CLaSH.Divide
import CLaSH.CRC

{-# ANN module ("HLint: ignore Avoid reverse") #-}

--BCD conversion testing
prop_BCDConversion = 
    forAll (choose (0, 9999)) $ \(x :: Int) -> --Make sure that the input range is representable within the output type
        dropWhile (==0) (toList (toDec (fromIntegral x :: BitVector 16) :: Vec 4 BCDDigit)) == Prelude.map fromIntegral (digits 10 x)

--FIR filter testing
--Check that both optimised filters return the same results as the unoptimised one
prop_FilterTransposed :: Vec 64 (Signed 32) -> [Signed 32] -> Bool
prop_FilterTransposed coeffs input = 
       Prelude.take (Prelude.length input) (simulate (fir coeffs (pure True)) input) 
    == Prelude.take (Prelude.length input) (Prelude.drop 1 (simulate (firTransposed (reverse coeffs) (pure True)) input))

prop_FilterLinearPhase :: Vec 64 (Signed 32) -> [Signed 32] -> Bool
prop_FilterLinearPhase coeffs input = 
       Prelude.take (Prelude.length input) (simulate (register 0 . fir (reverse coeffs ++ coeffs) (pure True)) input) 
    == Prelude.take (Prelude.length input) (simulate (firLinearPhase coeffs (pure True)) input)

--CORDIC testing
approxEqual :: Double -> Double -> Bool
approxEqual x y = abs (x - y) < 0.0001

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

--Bitonic sorting network
prop_BitonicSort :: Vec 16 (Signed 32) -> Bool
prop_BitonicSort vec = toList (bitonicSorterExample vec) == Prelude.reverse (Prelude.sort (toList vec))

--Divider
prop_Divider :: BitVector 32 -> BitVector 32 -> Bool
prop_Divider x y = q == q' && r == r'
    where
    (q, r)   = quotRem x y
    (q', r') = combDivide x y

--Check that the CRC implementation agrees with a known goood implementation of CRC32
revBV :: forall n. KnownNat n => BitVector n -> BitVector n
revBV = pack . reverse . (unpack :: BitVector n -> Vec n Bit)

reverseByte :: Word8 -> Word8
reverseByte = unpack . revBV . pack

toBytes :: BitVector 64 -> [Word8]
toBytes x = Prelude.map (fromIntegral . pack) $ toList unpacked
    where
    unpacked :: Vec 8 (Vec 8 Bit)
    unpacked = unconcatI (unpack x)

--The CRC32 polynomial
poly :: BitVector 31
poly = 0b10011000001000111011011011

prop_crc32 :: BitVector 64 -> Bool
prop_crc32 x = result == expect
    where
    expect = crc32 $ Prelude.map reverseByte (toBytes x)
    result = fromIntegral $ pack $ map complement $ reverse $ crcSteps poly (repeat 1) x

--Run the tests
return []
main :: IO ()
main = do
    success <- $quickCheckAll
    unless success exitFailure

