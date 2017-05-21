{-# LANGUAGE ScopedTypeVariables, GADTs #-}

{-| Radix 2 complex-to-complex Cooley-Tukey FFTs. https://en.wikipedia.org/wiki/Cooley%E2%80%93Tukey_FFT_algorithm.
    The FFTs in this module are fully parallel, which means they use a large number of multipliers and routing resources and are only practical for smaller FFTs.
-}
module CLaSH.FFT (
    twiddleFactors,
    halveTwiddles,
    reorder,
    fftDITRec,
    fftDIFRec,
    fftDITIter,
    fftDIFIter
    ) where

import CLaSH.Prelude

import CLaSH.Complex
import qualified Data.Complex as C
import qualified Prelude as P

twiddleFactors :: Int -> [Complex Double]
twiddleFactors num = P.take num $ [fromComplex $ C.cis $ (-1) * P.pi * fromIntegral i / (fromIntegral num) | i <- [0..]]

halveTwiddles :: KnownNat n => Vec (2 * n) a -> Vec n a
halveTwiddles vec = transpose (unconcat (SNat  @ 2) vec) !! 0

reorder :: forall n a. KnownNat n => Vec (2 ^ n) a -> Vec (2 ^ n) a
reorder inp = map (inp !!) indices
    where
    indices :: Vec (2 ^ n) (BitVector n)
    indices = map revBits $ iterateI (+1) 0
    revBits :: forall n. KnownNat n => BitVector n -> BitVector n
    revBits x = pack $ reverse (unpack x :: Vec n Bit)

fftStepDITRec 
    :: forall n a . (Num a, KnownNat n)
    => Vec n (Complex a)
    -> (Vec n (Complex a) -> Vec n (Complex a))
    -> Vec (2 * n) (Complex a)
    -> Vec (2 * n) (Complex a)
fftStepDITRec twiddleFactors recurse input = zipWith (+) fft1 twiddled ++ zipWith (-) fft1 twiddled
    where
    grouped     :: Vec n (Vec 2 (Complex a))
    grouped     =  unconcatI input
    partitioned :: Vec 2 (Vec n (Complex a))
    partitioned =  transpose grouped

    fft1        =  recurse $ partitioned !! 0
    fft2        =  recurse $ partitioned !! 1
    twiddled    =  zipWith (*) twiddleFactors fft2

fftStepDIFRec 
    :: forall n a . (Num a, KnownNat n)
    => Vec n (Complex a)
    -> (Vec n (Complex a) -> Vec n (Complex a))
    -> Vec (2 * n) (Complex a)
    -> Vec (2 * n) (Complex a)
fftStepDIFRec twiddleFactors recurse input = fft1 ++ fft2
    where
    partitioned :: Vec 2 (Vec n (Complex a))
    partitioned =  unconcatI input

    fft1        = recurse $ zipWith (+) (partitioned !! 0) (partitioned !! 1)
    fft2        = recurse $ zipWith (*) twiddleFactors $ zipWith (-) (partitioned !! 0) (partitioned !! 1)

--TODO: use dependently typed fold to automate all this
--A completely impractical purely combinational FFT
fftDITRec
    :: forall a. (Num a, Floating a)
    => Vec 4 (Complex a)
    -> Vec 8 (Complex a) 
    -> Vec 8 (Complex a)
fftDITRec twiddles = fft8 
    where
    cexp1 :: Vec 1 (Complex a)
    cexp1 = halveTwiddles cexp2

    cexp2 :: Vec 2 (Complex a)
    cexp2 = halveTwiddles cexp4

    cexp4 :: Vec 4 (Complex a)
    cexp4 = twiddles

    fft1 :: Vec 1 (Complex a) -> Vec 1 (Complex a)
    fft1 = id

    fft2 :: Vec 2 (Complex a) -> Vec 2 (Complex a)
    fft2 = fftStepDITRec cexp1 fft1

    fft4 :: Vec 4 (Complex a) -> Vec 4 (Complex a)
    fft4 = fftStepDITRec cexp2 fft2

    fft8 :: Vec 8 (Complex a) -> Vec 8 (Complex a)
    fft8 = fftStepDITRec cexp4 fft4

--TODO: use dependently typed fold to automate all this
--A completely impractical purely combinational FFT
fftDIFRec 
    :: forall a. (Num a, Floating a)
    => Vec 4 (Complex a)
    -> Vec 8 (Complex a) 
    -> Vec 8 (Complex a)
fftDIFRec twiddles input = reorder $ fft8 input
    where

    cexp1 :: Vec 1 (Complex a)
    cexp1 = halveTwiddles cexp2

    cexp2 :: Vec 2 (Complex a)
    cexp2 = halveTwiddles cexp4

    cexp4 :: Vec 4 (Complex a)
    cexp4 = twiddles

    fft1 :: Vec 1 (Complex a) -> Vec 1 (Complex a)
    fft1 = id

    fft2 :: Vec 2 (Complex a) -> Vec 2 (Complex a)
    fft2 = fftStepDIFRec cexp1 fft1

    fft4 :: Vec 4 (Complex a) -> Vec 4 (Complex a)
    fft4 = fftStepDIFRec cexp2 fft2

    fft8 :: Vec 8 (Complex a) -> Vec 8 (Complex a)
    fft8 = fftStepDIFRec cexp4 fft4

butterfly 
    :: forall n a. (Num a, KnownNat n)
    => Vec (2 * n) (Complex a)
    -> Vec (2 * n) (Complex a)
butterfly input = butterflied1 ++ butterflied2
    where
    partitioned  :: Vec 2 (Vec n (Complex a))
    partitioned  =  unconcatI input
    butterflied1 =  zipWith (+) (partitioned !! 0) (partitioned !! 1)
    butterflied2 =  zipWith (-) (partitioned !! 0) (partitioned !! 1)

twiddle 
    :: forall n a. (Num a, KnownNat n)
    => Vec n (Complex a)
    -> Vec (2 * n) (Complex a)
    -> Vec (2 * n) (Complex a)
twiddle twiddleFactors input = (partitioned !! 0) ++ twiddled
    where
    partitioned :: Vec 2 (Vec n (Complex a))
    partitioned =  unconcatI input
    twiddled    =  zipWith (*) twiddleFactors (partitioned !! 1)

fftDITIter 
    :: forall a. (Num a)
    => Vec 4 (Complex a)
    -> Vec 8 (Complex a)
    -> Vec 8 (Complex a)
fftDITIter twiddles inp = fft8
    where
    cexp1 :: Vec 1 (Complex a)
    cexp1 = halveTwiddles cexp2

    cexp2 :: Vec 2 (Complex a)
    cexp2 = halveTwiddles cexp4

    cexp4 :: Vec 4 (Complex a)
    cexp4 = twiddles

    fft2 = concat $ map (butterfly . twiddle cexp1) $ unconcatI $ reorder inp
    fft4 = concat $ map (butterfly . twiddle cexp2) $ unconcatI fft2
    fft8 = concat $ map (butterfly . twiddle cexp4) $ unconcatI fft4

fftDIFIter 
    :: forall a. (Num a)
    => Vec 4 (Complex a)
    -> Vec 8 (Complex a)
    -> Vec 8 (Complex a)
fftDIFIter twiddles input = reorder fft2
    where
    
    cexp1 :: Vec 1 (Complex a)
    cexp1 = halveTwiddles cexp2

    cexp2 :: Vec 2 (Complex a)
    cexp2 = halveTwiddles cexp4

    cexp4 :: Vec 4 (Complex a)
    cexp4 = twiddles

    fft8 = concat $ map (twiddle cexp4 . butterfly) $ unconcatI input
    fft4 = concat $ map (twiddle cexp2 . butterfly) $ unconcatI fft8
    fft2 = concat $ map (twiddle cexp1 . butterfly) $ unconcatI fft4

