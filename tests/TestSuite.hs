{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeOperators, NoImplicitPrelude, FlexibleContexts, TemplateHaskell, BinaryLiterals, RecordWildCards, TypeApplications, GADTs #-}

import Control.Monad
import System.Exit

import Test.QuickCheck
import Test.QuickCheck.All

import Data.Digits
import qualified Data.Complex as C
import Data.Digest.CRC32
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import qualified Numeric.FFT as FFT

import CLaSH.Prelude
import qualified Prelude
import qualified Data.List as Prelude
import Data.Word
import Data.Tuple.All
import Data.Maybe

import CLaSH.BCD
import CLaSH.FIRFilter
import CLaSH.IIRFilter
import CLaSH.CORDIC
import CLaSH.Sort
import CLaSH.Divide
import CLaSH.CRC
import CLaSH.FIFO
import CLaSH.GrayCode
import CLaSH.FFT
import CLaSH.FFTSerial
import CLaSH.Complex
import CLaSH.Hamming

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

prop_FilterSystolic :: Vec 64 (Signed 32) -> [Signed 32] -> Bool
prop_FilterSystolic coeffs input = 
       Prelude.take (Prelude.length input) (simulate (fir coeffs (pure True)) input) 
    == Prelude.take (Prelude.length input) (Prelude.drop 64 $ simulate (firSystolic coeffs (pure True)) input)

prop_FilterSystolicSymmetric :: Vec 64 (Signed 32) -> [Signed 32] -> Bool
prop_FilterSystolicSymmetric coeffs input = 
       Prelude.take (Prelude.length input) (simulate (fir (coeffs ++ reverse coeffs) (pure True)) input) 
    == Prelude.take (Prelude.length input) (Prelude.drop 64 $ simulate (firSystolicSymmetric coeffs (pure True)) input)

prop_FilterSymmetric :: Vec 64 (Signed 32) -> [Signed 32] -> Bool
prop_FilterSymmetric coeffs input = 
       Prelude.take (Prelude.length input) (simulate (register 0 . fir (reverse coeffs ++ coeffs) (pure True)) input) 
    == Prelude.take (Prelude.length input) (simulate (firSymmetric coeffs (pure True)) input)

prop_FilterTransposedSymmetric :: Vec 64 (Signed 32) -> [Signed 32] -> Bool
prop_FilterTransposedSymmetric coeffs input = 
       Prelude.take (Prelude.length input) (simulate (fir (coeffs ++ reverse coeffs) (pure True)) input) 
    == Prelude.take (Prelude.length input) (Prelude.drop 1 $ simulate (firTransposedSymmetric coeffs (pure True)) input)

--IIR filter testing
--Check that both direct forms are equivalent
prop_IIRDirect :: Vec 65 (Signed 32) -> Vec 64 (Signed 32) -> [Signed 32] -> Bool
prop_IIRDirect coeffs1 coeffs2 input = 
       Prelude.take (Prelude.length input) (simulate (iirDirectI coeffs1 coeffs2 (pure True)) input) 
    == Prelude.take (Prelude.length input) (simulate (iirDirectII coeffs1 coeffs2 (pure True)) input)

prop_IIRTransposedI :: Vec 65 (Signed 32) -> Vec 64 (Signed 32) -> [Signed 32] -> Bool
prop_IIRTransposedI coeffs1 coeffs2 input = 
       Prelude.take (Prelude.length input) (simulate (iirDirectI     coeffs1 coeffs2 (pure True)) input) 
    == Prelude.take (Prelude.length input) (simulate (iirTransposedI coeffs1 coeffs2 (pure True)) input)

prop_IIRTransposedII :: Vec 65 (Signed 32) -> Vec 64 (Signed 32) -> [Signed 32] -> Bool
prop_IIRTransposedII coeffs1 coeffs2 input = 
       Prelude.take (Prelude.length input) (simulate (iirDirectI     coeffs1 coeffs2 (pure True)) input) 
    == Prelude.take (Prelude.length input) (simulate (iirTransposedII coeffs1 coeffs2 (pure True)) input)

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

--Length generic bitonic sorter
prop_BitonicSortGeneric :: Vec 64 (Signed 32) -> Bool
prop_BitonicSortGeneric vec = toList (bitonicSorter vec) == Prelude.reverse (Prelude.sort (toList vec))

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

toBytes :: forall n. KnownNat n => BitVector (n * 8) -> [Word8]
toBytes x = Prelude.map (fromIntegral . pack) $ toList unpacked
    where
    unpacked :: Vec n (Vec 8 Bit)
    unpacked = unconcatI (unpack x)

prop_crc32 :: BitVector 128 -> Bool
prop_crc32 x = result == expect
    where
    expect = crc32 $ Prelude.map reverseByte (toBytes x)
    result = fromIntegral $ pack $ map complement $ reverse $ crcSteps crc32Poly (repeat 1) x

prop_crc32_2 :: BitVector 128 -> Bool
prop_crc32_2 x = result == expect
    where
    expect = crcSteps       crc32Poly (repeat 0) x
    result = crcVerifySteps crc32Poly (repeat 0) $ x ++# (0 :: BitVector 32)

prop_crc32_verify :: BitVector 128 -> Bool
prop_crc32_verify x = result == 0
    where
    checksum = pack $ crcSteps       crc32Poly (repeat 0) x
    result   = pack $ crcVerifySteps crc32Poly (repeat 0) $ x ++# checksum

prop_crc32_table :: BitVector 128 -> Bool
prop_crc32_table x = result == expect
    where
    expect = pack $ crcSteps crc32Poly (repeat 0) x
    result = crcTable (makeCRCTable (pack . crcSteps crc32Poly (repeat 0))) x

prop_crc32_table_verify :: BitVector 128 -> Bool
prop_crc32_table_verify x = result == expect
    where
    expect = pack $ crcVerifySteps crc32Poly (repeat 0) x
    result = crcTable (makeCRCTable (pack . crcVerifySteps crc32Poly (repeat 0))) x

prop_crc32_multistep :: BitVector 256 -> Bool
prop_crc32_multistep x = unpack result == expect
    where
    expect = pack $ crcSteps crc32Poly (repeat 0) x
    step :: BitVector 32 ->  BitVector 32 -> BitVector 32
    step   = crcTableMultiStep shiftRegTable inputTable
        where 
        (shiftRegTable, inputTable) = makeCRCTableMultiStep (\x y -> pack $ crcSteps crc32Poly (unpack x) y)
    words :: Vec 8 (BitVector 32)
    words = unpack x 
    result :: BitVector 32
    result = foldl step 0 words

prop_crc32_multistep_2 :: BitVector 256 -> Bool
prop_crc32_multistep_2 x = unpack result == expect
    where
    expect = crc32 $ Prelude.map reverseByte (toBytes x)
    step :: BitVector 32 ->  BitVector 32 -> BitVector 32
    step   = crcTableMultiStep shiftRegTable inputTable
        where 
        (shiftRegTable, inputTable) = makeCRCTableMultiStep (\x y -> pack $ crcSteps crc32Poly (unpack x) y)
    words :: Vec 8 (BitVector 32)
    words = unpack x 
    result :: BitVector 32
    result = fromIntegral $ pack $ map complement $ reverse $ (unpack $ foldl step 0xffffffff words :: Vec 32 Bit)

prop_crc32_multistep_verify :: BitVector 256 -> Bool
prop_crc32_multistep_verify x = unpack result == expect
    where
    expect = pack $ crcVerifySteps crc32Poly (repeat 0) x
    step :: BitVector 32 ->  BitVector 32 -> BitVector 32
    step   = crcTableMultiStep shiftRegTable inputTable
        where 
        (shiftRegTable, inputTable) = makeCRCTableMultiStep (\x y -> pack $ crcVerifySteps crc32Poly (unpack x) y)
    words :: Vec 8 (BitVector 32)
    words = unpack x 
    result :: BitVector 32
    result = foldl step 0 words

--FIFO
--This software model should behave identically to the hardare FIFO
fifoStep :: Int -> Seq a -> (Bool, a, Bool) -> (Seq a, (a, Bool, Bool))
fifoStep size storage (readEn, writeValue, writeEn) = (storage'', (extract storage, empty, full)) 
    where
    full             = Seq.length storage == size - 1
    empty            = Seq.length storage == 0
    --Do the read operation before the write
    storage'
        | readEn     = Seq.drop 1 storage
        | otherwise  = storage 
    storage''
        | writeEn && not full = storage' |> writeValue
        | otherwise           = storage'
    extract :: Seq a -> a
    extract s = case Seq.viewl s of
        (x Seq.:< rest) -> x

compareOutputs :: Eq a => (a, Bool, Bool) -> (a, Bool, Bool) -> Bool
compareOutputs (val1, empty1, full1) (val2, empty2, full2) 
    =  empty1 == empty2 
    && full1  == full2 
    && (empty1 || val1 == val2)

prop_FIFOs :: [(Bool, BitVector 32, Bool)] -> Bool
prop_FIFOs signals = Prelude.and $ Prelude.zipWith compareOutputs expect result
    where
    expect = Prelude.take (Prelude.length signals) $ simulate_lazy (mealy (fifoStep 5) Seq.empty) signals
    result = Prelude.take (Prelude.length signals) $ simulate_lazy hackedFIFO signals
    hackedFIFO :: Signal (Bool, BitVector 32, Bool) -> Signal (BitVector 32, Bool, Bool)
    hackedFIFO = bundle . uncurryN (blockRamFIFO (SNat @ 5)) . unbundle 

prop_FIFOMaybe :: [(Bool, BitVector 32, Bool)] -> Bool
prop_FIFOMaybe signals = Prelude.and $ Prelude.zipWith compareOutputs expect result
    where
    expect = Prelude.take (Prelude.length signals) $ simulate_lazy (mealy (fifoStep 5) Seq.empty) signals
    result = Prelude.take (Prelude.length signals) $ simulate_lazy hackedFIFO signals
    hackedFIFO :: Signal (Bool, BitVector 32, Bool) -> Signal (BitVector 32, Bool, Bool)
    hackedFIFO inputs = bundle $ (fromJust <$> readDataM, (not . isJust) <$> readDataM, full)
        where
        (readReq, writeData, writeReq) = unbundle inputs
        (readDataM, full)              = blockRamFIFOMaybe (SNat @ 5) readReq $ mux writeReq (Just <$> writeData) (pure Nothing)

--Gray code
prop_grayCode :: BitVector 32 -> Bool
prop_grayCode x = x == binaryToGray (grayToBinary x)

prop_grayCode2 :: BitVector 32 -> Bool
prop_grayCode2 x = x == grayToBinary (binaryToGray x)

--FFT
twiddles :: Vec 8 (Complex Double)
twiddles = $(listToVecTH (twiddleFactors 8))

approxEqualComplex (a C.:+ b) (c C.:+ d) = approxEqual a c && approxEqual b d

prop_fftDITRec :: Vec 16 (C.Complex Double) -> Bool
prop_fftDITRec vec = and $ Prelude.zipWith approxEqualComplex (Prelude.map toComplex (toList (fftDITRec twiddles (map fromComplex vec)))) (FFT.fft (toList vec))

prop_fftDIFRec :: Vec 16 (C.Complex Double) -> Bool
prop_fftDIFRec vec = and $ Prelude.zipWith approxEqualComplex (Prelude.map toComplex (toList (fftDIFRec twiddles (map fromComplex vec)))) (FFT.fft (toList vec))

prop_fftDITIter :: Vec 16 (C.Complex Double) -> Bool
prop_fftDITIter vec = and $ Prelude.zipWith approxEqualComplex (Prelude.map toComplex (toList (fftDITIter twiddles (map fromComplex vec)))) (FFT.fft (toList vec))

prop_fftDIFIter :: Vec 16 (C.Complex Double) -> Bool
prop_fftDIFIter vec = and $ Prelude.zipWith approxEqualComplex (Prelude.map toComplex (toList (fftDIFIter twiddles (map fromComplex vec)))) (FFT.fft (toList vec))

--serial FFT
twiddles4 :: Vec 4 (Complex Double)
twiddles4 = $(listToVecTH (twiddleFactors 4))

ditInputReorder :: Vec 8 a -> Vec 4 (a, a)
ditInputReorder (a :> b :> c :> d :> e :> f :> g :> h :> Nil) = (a, e) :> (c, g) :> (b, f) :> (d, h) :> Nil

ditOutputReorder :: [(a, a)] -> [a]
ditOutputReorder ((a, b) : (c, d) : (e, f) : (g, h) : _) = a : c : e : g : b : d : f : h : []

difInputReorder :: Vec 8 a -> Vec 4 (a, a)
difInputReorder (a :> b :> c :> d :> e :> f :> g :> h :> Nil) = (a, e) :> (b, f) :> (c, g) :> (d, h) :> Nil

difOutputReorder :: [(a, a)] -> [a]
difOutputReorder ((a, b) : (c, d) : (e, f) : (g, h) : _) = a : e : c : g : b : f : d : h : []

prop_fftSerial :: Vec 8 (C.Complex Double) -> Bool
prop_fftSerial vec = and $ Prelude.zipWith approxEqualComplex (Prelude.map toComplex result) (FFT.fft (toList vec))
    where
    result = ditOutputReorder $ Prelude.drop 8 $ simulate_lazy (fftSerial twiddles4 (pure True)) $ (toList (ditInputReorder (map fromComplex vec))) Prelude.++ Prelude.repeat (0, 0)

prop_fftSerialDIF :: Vec 8 (C.Complex Double) -> Bool
prop_fftSerialDIF vec = and $ Prelude.zipWith approxEqualComplex (Prelude.map toComplex result) (FFT.fft (toList vec))
    where
    result = difOutputReorder $ Prelude.drop 8 $ simulate_lazy (fftSerialDIF twiddles4 (pure True)) $ (toList (difInputReorder (map fromComplex vec))) Prelude.++ Prelude.repeat (0, 0)

--Hamming codes
--Test the (15, 11) code

hammingGen :: Vec 11 (BitVector 4)
hammingGen = map pack $ unconcatI $ $(listToVecTH $ Prelude.concat $ Prelude.take 11 $ Prelude.map (Prelude.take 4) generator)

prop_hamming :: Index 15 -> BitVector 11 -> Bool
prop_hamming mutIdx dat = dat == corrected
    where
    --encode
    parityBits  = hammingParity hammingGen dat
    encoded     = dat ++# parityBits
    --flip a single bit
    mutated     = complementBit encoded (fromIntegral mutIdx)
    --decode
    parityBits' = hammingParity hammingGen (slice d14 d4 mutated) `xor` (slice d3 d0 mutated)
    corrected   = correctError hammingGen parityBits' (slice d14 d4 mutated)
        
--Run the tests
return []
main :: IO ()
main = do
    success <- $quickCheckAll
    unless success exitFailure

