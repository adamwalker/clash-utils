{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeOperators, FlexibleContexts, TemplateHaskell, BinaryLiterals, RecordWildCards, TypeApplications, GADTs #-}

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

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), HasClockReset, mealy, mux, bundle, unbundle, SNat(..))
import GHC.TypeLits
import Data.Word
import Data.Tuple.All
import Data.Maybe
import Data.List
import qualified Data.List.Split as S
import Data.Serialize (runGet, runPut)

import Test.Hspec

import Clash.BCD
import Clash.FIRFilter
import Clash.IIRFilter
import Clash.CORDIC
import Clash.Sort
import Clash.Divide
import Clash.CRC
import Clash.FIFO
import Clash.GrayCode
import Clash.FFT
import Clash.FFTSerial
import Clash.Complex
import Clash.Hamming
import Clash.Misc
import Clash.Scrambler
import Clash.Serialize
import Clash.PseudoLRUTree

{-# ANN module ("HLint: ignore Avoid reverse") #-}

main = hspec $ do
    bcdSpec
    firFilterSpec
    iirFilterSpec
    cordicSpec
    bitonicSortSpec
    dividerSpec
    crcSpec
    fifoSpec
    grayCodeSpec
    parallelFFTSpec
    serialFFTSpec
    hammingSpec
    miscSpec
    scramblerSpec
    serializeSpec
    plruSpec

--BCD conversion testing
bcdSpec = describe "Binary to decimal conversion using the double dabble algorithm" $ 
    it "correctly converts binary numbers between 0 and 9999 to decimal" $ property $ 
        forAll (choose (0, 9999)) $ \(x :: Int) -> --Make sure that the input range is representable within the output type
            dropWhile (==0) (Clash.toList (toDec (fromIntegral x :: BitVector 16) :: Vec 4 BCDDigit)) == map fromIntegral (digits 10 x)

--FIR filter testing
--Check that both optimised filters return the same results as the unoptimised one
firFilterSpec = describe "FIR filters" $ do
    describe "Parallel" $ do
        specify "transposed"          $ property $ prop_FilterTransposed
        specify "systolic"            $ property $ prop_FilterSystolic
        specify "systolic symmetric"  $ property $ prop_FilterSystolicSymmetric
        specify "symmetric"           $ property $ prop_FilterSymmetric
        specify "transposed symetric" $ property $ prop_FilterTransposedSymmetric
        specify "systolic symmetric"  $ property $ prop_systolicSymmetric
        specify "systolic half band"  $ property $ prop_systolicHalfBand
    describe "Semi-parallel" $ do
        specify "semi parallel 1"     $ property $ prop_semiParallelFIR1
        specify "semi parallel 2"     $ property $ prop_semiParallelFIR2
        specify "semi parallel 3"     $ property $ prop_semiParallelFIR3

    where

    prop_FilterTransposed :: Vec 16 (Signed 32) -> [Signed 32] -> Bool
    prop_FilterTransposed coeffs input = 
           take (length input) (simulate (fir coeffs (pure True)) input) 
        == take (length input) (drop 1 (simulate (firTransposed (Clash.reverse coeffs) (pure True)) input))

    prop_FilterSystolic :: Vec 16 (Signed 32) -> [Signed 32] -> Bool
    prop_FilterSystolic coeffs input = 
           take (length input) (simulate (fir coeffs (pure True)) input) 
        == take (length input) (drop 16 $ simulate (firSystolic coeffs (pure True)) input)

    prop_FilterSystolicSymmetric :: Vec 16 (Signed 32) -> [Signed 32] -> Bool
    prop_FilterSystolicSymmetric coeffs input = 
           take (length input) (simulate (fir (coeffs Clash.++ Clash.reverse coeffs) (pure True)) input) 
        == take (length input) (drop 16 $ simulate (firSystolicSymmetric coeffs (pure True)) input)

    prop_FilterSymmetric :: Vec 16 (Signed 32) -> [Signed 32] -> Bool
    prop_FilterSymmetric coeffs input = 
           take (length input) (simulate (Clash.register 0 . fir (Clash.reverse coeffs Clash.++ coeffs) (pure True)) input) 
        == take (length input) (simulate (firSymmetric coeffs (pure True)) input)

    prop_FilterTransposedSymmetric :: Vec 64 (Signed 32) -> [Signed 32] -> Bool
    prop_FilterTransposedSymmetric coeffs input = 
           take (length input) (simulate (fir (coeffs Clash.++ Clash.reverse coeffs) (pure True)) input) 
        == take (length input) (drop 1 $ simulate (firTransposedSymmetric coeffs (pure True)) input)

    prop_systolicSymmetric :: Vec 16 (Signed 32) -> Signed 32 -> [Signed 32] -> Bool
    prop_systolicSymmetric coeffs mid input = 
           take (length input) (simulate (fir (coeffs Clash.++ Clash.singleton mid Clash.++ Clash.reverse coeffs) (pure True)) input)
        == take (length input) (drop 17 $ simulate (firSystolicSymmetricOdd (coeffs Clash.++ Clash.singleton mid) (pure True)) input)

    prop_systolicHalfBand :: Vec 16 (Signed 32) -> Signed 32 -> [Signed 32] -> Bool
    prop_systolicHalfBand coeffs mid input = 
           take (length input) (simulate (fir (coeffs' Clash.++ Clash.singleton mid Clash.++ Clash.reverse coeffs') (pure True)) input)
        == take (length input) (drop 17 $ simulate (firSystolicHalfBand (coeffs Clash.++ Clash.singleton mid) (pure True)) input)
        where
        coeffs' = Clash.init (Clash.merge coeffs (Clash.repeat 0))

    --Semi-parallel FIR filter has lots of tests because it is confusing
    prop_semiParallelFIR1 :: Vec 9 (Signed 32) -> [Signed 32] -> Bool
    prop_semiParallelFIR1 coeffs input = res1 == res2
        where
        coeffs2 :: Vec 3 (Vec 3 (Signed 32))
        coeffs2 =  Clash.unconcatI coeffs
        input'  =  input ++ repeat 0
        res1    =  take 50 $ simulate (fir coeffs (pure True)) input'
        res2    =  take 50 $ map head $ S.chunksOf 3 $ drop 9 $ simulate (semiParallelFIR coeffs2 (pure True)) (concatMap ((++ [0, 0]) . pure) input' ++ repeat 0)

    prop_semiParallelFIR2 :: Vec 15 (Signed 32) -> [Signed 32] -> Bool
    prop_semiParallelFIR2 coeffs input = res1 == res2
        where
        coeffs2 :: Vec 5 (Vec 3 (Signed 32))
        coeffs2 =  Clash.unconcatI coeffs
        input'  =  input ++ repeat 0
        res1    =  take 50 $ simulate (fir coeffs (pure True)) input'
        res2    =  take 50 $ map head $ S.chunksOf 3 $ drop 11 $ simulate (semiParallelFIR coeffs2 (pure True)) (concatMap ((++ [0, 0]) . pure) input' ++ repeat 0)

    prop_semiParallelFIR3 :: Vec 20 (Signed 32) -> [Signed 32] -> Bool
    prop_semiParallelFIR3 coeffs input = res1 == res2
        where
        coeffs2 :: Vec 5 (Vec 4 (Signed 32))
        coeffs2 =  Clash.unconcatI coeffs
        input'  =  input ++ repeat 0
        res1    =  take 50 $ simulate (fir coeffs (pure True)) input'
        res2    =  take 50 $ map head $ S.chunksOf 4 $ drop 12 $ simulate (semiParallelFIR coeffs2 (pure True)) (concatMap ((++ [0, 0, 0]) . pure) input' ++ repeat 0)

--IIR filter testing
--Check that both direct forms are equivalent
iirFilterSpec = describe "IIR filters" $ do
    specify "direct"       $ property prop_IIRDirect
    specify "transposedI"  $ property prop_IIRTransposedI
    specify "transposedII" $ property prop_IIRTransposedII

    where
    prop_IIRDirect :: Vec 65 (Signed 32) -> Vec 64 (Signed 32) -> [Signed 32] -> Bool
    prop_IIRDirect coeffs1 coeffs2 input = 
           take (length input) (simulate (iirDirectI coeffs1 coeffs2 (pure True)) input) 
        == take (length input) (simulate (iirDirectII coeffs1 coeffs2 (pure True)) input)

    prop_IIRTransposedI :: Vec 65 (Signed 32) -> Vec 64 (Signed 32) -> [Signed 32] -> Bool
    prop_IIRTransposedI coeffs1 coeffs2 input = 
           take (length input) (simulate (iirDirectI     coeffs1 coeffs2 (pure True)) input) 
        == take (length input) (simulate (iirTransposedI coeffs1 coeffs2 (pure True)) input)

    prop_IIRTransposedII :: Vec 65 (Signed 32) -> Vec 64 (Signed 32) -> [Signed 32] -> Bool
    prop_IIRTransposedII coeffs1 coeffs2 input = 
           take (length input) (simulate (iirDirectI     coeffs1 coeffs2 (pure True)) input) 
        == take (length input) (simulate (iirTransposedII coeffs1 coeffs2 (pure True)) input)

approxEqual :: Double -> Double -> Bool
approxEqual x y = abs (x - y) < 0.0001

--CORDIC testing
cordicSpec = describe "CORDIC" $ do
    specify "vector mode"    $ property prop_CORDICVectorMode
    specify "rotation mode " $ property prop_CORDICRotationMode

    where
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
bitonicSortSpec = describe "Bitonic sort" $ do
    specify "sorts"          $ property prop_BitonicSort
    specify "sorts any size" $ property prop_BitonicSortGeneric

    where
    prop_BitonicSort :: Vec 16 (Signed 32) -> Bool
    prop_BitonicSort vec = Clash.toList (bitonicSorterExample vec) == reverse (sort (Clash.toList vec))

    --Length generic bitonic sorter
    prop_BitonicSortGeneric :: Vec 64 (Signed 32) -> Bool
    prop_BitonicSortGeneric vec = Clash.toList (bitonicSorter vec) == reverse (sort (Clash.toList vec))

--Divider
dividerSpec = describe "Divder" $ 
    specify "divides" $ property prop_Divider
    
    where
    prop_Divider :: BitVector 32 -> BitVector 32 -> Bool
    prop_Divider x y = q == q' && r == r'
        where
        (q, r)   = quotRem x y
        (q', r') = combDivide x y

--Check that the CRC implementation agrees with a known goood implementation of CRC32
crcSpec = describe "CRC" $ do
    specify "matches known good implementation"      $ property prop_crc32
    specify "equals crc verification of zero padded" $ property prop_crc32_2
    specify "verification"                           $ property prop_crc32_verify
    specify "table based implementation"             $ property prop_crc32_table
    specify "table based implementation of verify"   $ property prop_crc32_table_verify
    specify "compute in multiple steps"              $ property prop_crc32_multistep
    specify "compute in multiple steps 2"            $ property prop_crc32_multistep_2
    specify "compute verify in multiple steps"       $ property prop_crc32_multistep_verify
    
    where
    reverseByte :: Word8 -> Word8
    reverseByte = unpack . revBV . pack

    toBytes :: forall n. KnownNat n => BitVector (n * 8) -> [Word8]
    toBytes x = map (fromIntegral . pack) $ Clash.toList unpacked
        where
        unpacked :: Vec n (Vec 8 Bit)
        unpacked = Clash.unconcatI (unpack x)

    prop_crc32 :: BitVector 128 -> Bool
    prop_crc32 x = result == expect
        where
        expect = crc32 $ map reverseByte (toBytes x)
        result = fromIntegral $ pack $ Clash.map Clash.complement $ Clash.reverse $ crcSteps crc32Poly (Clash.repeat 1) x

    prop_crc32_2 :: BitVector 128 -> Bool
    prop_crc32_2 x = result == expect
        where
        expect = crcSteps       crc32Poly (Clash.repeat 0) x
        result = crcVerifySteps crc32Poly (Clash.repeat 0) $ x ++# (0 :: BitVector 32)

    prop_crc32_verify :: BitVector 128 -> Bool
    prop_crc32_verify x = result == 0
        where
        checksum = pack $ crcSteps       crc32Poly (Clash.repeat 0) x
        result   = pack $ crcVerifySteps crc32Poly (Clash.repeat 0) $ x ++# checksum

    prop_crc32_table :: BitVector 128 -> Bool
    prop_crc32_table x = result == expect
        where
        expect = pack $ crcSteps crc32Poly (Clash.repeat 0) x
        result = crcTable (makeCRCTable (pack . crcSteps crc32Poly (Clash.repeat 0))) x

    prop_crc32_table_verify :: BitVector 128 -> Bool
    prop_crc32_table_verify x = result == expect
        where
        expect = pack $ crcVerifySteps crc32Poly (Clash.repeat 0) x
        result = crcTable (makeCRCTable (pack . crcVerifySteps crc32Poly (Clash.repeat 0))) x

    prop_crc32_multistep :: BitVector 256 -> Bool
    prop_crc32_multistep x = unpack result == expect
        where
        expect = pack $ crcSteps crc32Poly (Clash.repeat 0) x
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
        result = fromIntegral $ pack $ Clash.map Clash.complement $ Clash.reverse $ (unpack $ foldl step 0xffffffff words :: Vec 32 Bit)

    prop_crc32_multistep_verify :: BitVector 256 -> Bool
    prop_crc32_multistep_verify x = unpack result == expect
        where
        expect = pack $ crcVerifySteps crc32Poly (Clash.repeat 0) x
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
fifoSpec = describe "FIFO" $ do
    specify "equivalent to software model" $ property prop_FIFOs
    specify "maybe version equivalent to software model" $ property prop_FIFOMaybe

    where
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
    prop_FIFOs signals = and $ zipWith compareOutputs expect result
        where
        expect = take (length signals) $ simulate_lazy (mealy (fifoStep 5) Seq.empty) signals
        result = take (length signals) $ simulate_lazy hackedFIFO signals
        hackedFIFO :: HasClockReset dom gated sync => Signal dom (Bool, BitVector 32, Bool) -> Signal dom (BitVector 32, Bool, Bool)
        hackedFIFO = bundle . uncurryN (blockRamFIFO (SNat @ 5)) . unbundle 

    prop_FIFOMaybe :: [(Bool, BitVector 32, Bool)] -> Bool
    prop_FIFOMaybe signals = Prelude.and $ Prelude.zipWith compareOutputs expect result
        where
        expect = take (length signals) $ simulate_lazy (mealy (fifoStep 5) Seq.empty) signals
        result = take (length signals) $ simulate_lazy hackedFIFO signals
        hackedFIFO :: HasClockReset dom gated sync => Signal dom (Bool, BitVector 32, Bool) -> Signal dom (BitVector 32, Bool, Bool)
        hackedFIFO inputs = bundle $ (fromJust <$> readDataM, (not . isJust) <$> readDataM, full)
            where
            (readReq, writeData, writeReq) = unbundle inputs
            (readDataM, full)              = blockRamFIFOMaybe (SNat @ 5) readReq $ mux writeReq (Just <$> writeData) (pure Nothing)

--Gray code
grayCodeSpec = describe "Gray code" $ do
    specify "binaryToGray . grayToBinary == id" $ property prop_grayCode
    specify "grayToBinary . binaryToGray == id" $ property prop_grayCode2

    where
    prop_grayCode :: BitVector 32 -> Bool
    prop_grayCode x = x == binaryToGray (grayToBinary x)

    prop_grayCode2 :: BitVector 32 -> Bool
    prop_grayCode2 x = x == grayToBinary (binaryToGray x)

approxEqualComplex (a C.:+ b) (c C.:+ d) = approxEqual a c && approxEqual b d

--FFT
parallelFFTSpec = describe "Parallel FFTs" $ do
    specify "Recursive decimation in time equals known good implementation"      $ property prop_fftDITRec
    specify "Recursive decimation in frequency equals known good implementation" $ property prop_fftDIFRec
    specify "Iterative decimation in time equals known good implementation"      $ property prop_fftDITIter
    specify "Iterative decimtaion in frequency equals known good implementation" $ property prop_fftDIFIter

    where
    twiddles :: Vec 8 (Complex Double)
    twiddles = $(listToVecTH (twiddleFactors 8))

    prop_fftDITRec :: Vec 16 (C.Complex Double) -> Bool
    prop_fftDITRec vec = and $ zipWith approxEqualComplex (map toComplex (Clash.toList (fftDITRec twiddles (Clash.map fromComplex vec)))) (FFT.fft (Clash.toList vec))

    prop_fftDIFRec :: Vec 16 (C.Complex Double) -> Bool
    prop_fftDIFRec vec = and $ zipWith approxEqualComplex (map toComplex (Clash.toList (fftDIFRec twiddles (Clash.map fromComplex vec)))) (FFT.fft (Clash.toList vec))

    prop_fftDITIter :: Vec 16 (C.Complex Double) -> Bool
    prop_fftDITIter vec = and $ zipWith approxEqualComplex (map toComplex (Clash.toList (fftDITIter twiddles (Clash.map fromComplex vec)))) (FFT.fft (Clash.toList vec))

    prop_fftDIFIter :: Vec 16 (C.Complex Double) -> Bool
    prop_fftDIFIter vec = and $ zipWith approxEqualComplex (map toComplex (Clash.toList (fftDIFIter twiddles (Clash.map fromComplex vec)))) (FFT.fft (Clash.toList vec))


--serial FFT
serialFFTSpec = describe "Serial FFTs" $ do
    specify "Decimation in time equals known good implementation"      $ property prop_fftSerialDIT
    specify "Decimation in frequency equals known good implementation" $ property prop_fftSerialDIF

    where
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

    prop_fftSerialDIT :: Vec 8 (C.Complex Double) -> Bool
    prop_fftSerialDIT vec = and $ zipWith approxEqualComplex (map toComplex result) (FFT.fft (Clash.toList vec))
        where
        result = ditOutputReorder $ drop 8 $ simulate_lazy (fftSerialDIT twiddles4 (pure True)) $ (Clash.toList (ditInputReorder (Clash.map fromComplex vec))) ++ repeat (0, 0)

    prop_fftSerialDIF :: Vec 8 (C.Complex Double) -> Bool
    prop_fftSerialDIF vec = and $ zipWith approxEqualComplex (map toComplex result) (FFT.fft (Clash.toList vec))
        where
        result = difOutputReorder $ drop 8 $ simulate_lazy (fftSerialDIF twiddles4 (pure True)) $ (Clash.toList (difInputReorder (Clash.map fromComplex vec))) ++ repeat (0, 0)

--Hamming codes
--Test the (15, 11) code

hammingSpec = describe "Hamming encoding/decoding" $ do
    specify "decode inverse encode" $ property prop_hamming

    where
    hammingGen :: Vec 11 (BitVector 4)
    hammingGen = Clash.map pack $ Clash.unconcatI $ $(listToVecTH $ concat $ take 11 $ map (take 4) generator)

    prop_hamming :: Index 15 -> BitVector 11 -> Bool
    prop_hamming mutIdx dat = dat == corrected
        where
        --encode
        parityBits  = hammingParity hammingGen dat
        encoded     = dat ++# parityBits
        --flip a single bit
        mutated     = Clash.complementBit encoded (fromIntegral mutIdx)
        --decode
        corrected   = correctError hammingGen (Clash.slice (SNat @ 3) (SNat @ 0) mutated) (Clash.slice (SNat @ 14) (SNat @ 4) mutated)

--Misc
miscSpec = describe "Misc utilities" $ do
    specify "slice equals software implementation"               $ property prop_slice
    specify "reversing bitvector twice gives original bitvector" $ property prop_revBV
    specify "swapping endianness twice gives original bitvector" $ property prop_swapEndian

    where
    refSlice :: Int -> [a] -> [a] -> [a]
    refSlice idx dat vec
        | idx + length dat > length vec
            = take idx vec ++ take (length vec - idx) dat
        | otherwise
            = take idx vec ++ dat ++ drop (idx + length dat) vec

    prop_slice :: Index 253 -> Vec 21 Int -> Vec 253 Int -> Bool
    prop_slice startIdx dat vec = Clash.toList (replaceSlice startIdx dat vec) == refSlice (fromIntegral startIdx) (Clash.toList dat) (Clash.toList vec)

    prop_revBV :: BitVector 253 -> Bool
    prop_revBV x = x == revBV (revBV x)

    prop_swapEndian :: BitVector 256 -> Bool
    prop_swapEndian x = x == swapEndian (swapEndian x)

--Scrambler
scramblerSpec = describe "Scrambler" $ 
    specify "descramber . scrambler == id" $ property prop_scrambler

    where
    prop_scrambler :: BitVector 20 -> BitVector 19 -> [Bool] -> Bool
    prop_scrambler initial (poly :: BitVector 19) input = take (length input) (simulate combined input) == input
        where
        combined :: HasClockReset dom gated sync => Signal dom Bool -> Signal dom Bool
        combined s = descrambler initial poly $ scrambler initial poly s

--BitVector serialization
serializeSpec = describe "Serialize/deserialize" $ 
    specify "deserialize . serialize = id" $ property prop_serialize

    where
    prop_serialize :: BitVector 256 -> Bool
    prop_serialize bv = Right bv == result
        where
        result = runGet getBV $ runPut $ putBV bv

--Pseudu lru tree pseudo-tests
plruSpec = describe "Pseudo LRU tree" $ do
    specify "cycles through all possibilities" $ property prop_plru
    specify "both implementations same"        $ property prop_plruSame
    specify "update same index idempotent"     $ property prop_plruIdempotent
    specify "simple case with single bit"      $ property prop_plruSimpleCase

    where
    prop_plru :: Vec 15 Bool -> Bool
    prop_plru tree = reordered == [0..15]
        where
        trees     = Clash.iterate (SNat @ 16) func tree
            where
            func tree = updateWay (getOldestWay tree) tree
        reordered = sort $ Clash.toList $ Clash.map (fromIntegral . pack) $ Clash.map getOldestWay trees

    prop_plruSame :: Vec 15 Bool -> Bool
    prop_plruSame tree = updateOldestWay tree == (oldest, newTree)
        where
        oldest  = getOldestWay tree
        newTree = updateWay oldest tree

    prop_plruIdempotent :: Vec 15 Bool -> Vec 4 Bool -> Bool
    prop_plruIdempotent tree idx = updateWay idx tree == updateWay idx (updateWay idx tree)

    prop_plruSimpleCase :: Vec 1 Bool -> Bool
    prop_plruSimpleCase tree = updateWay (getOldestWay tree) tree == Clash.map not tree

