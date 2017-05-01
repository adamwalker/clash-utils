{-# LANGUAGE ScopedTypeVariables, GADTs #-}

module CLaSH.FFT (
    twiddleFactors,
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
    cexp1 = selectI (SNat @ 0) (SNat @ 4) twiddles

    cexp2 :: Vec 2 (Complex a)
    cexp2 = selectI (SNat @ 0) (SNat @ 2) twiddles

    cexp4 :: Vec 4 (Complex a)
    cexp4 = selectI (SNat @ 0) (SNat @ 1) twiddles

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
fftDIFRec twiddles input = (a :> e :> c :> g :> b :> f :> d :> h :> Nil)
    where

    (a :> b :> c :> d :> e :> f :> g :> h :> Nil) = fft8 input

    cexp1 :: Vec 1 (Complex a)
    cexp1 = selectI (SNat @ 0) (SNat @ 4) twiddles

    cexp2 :: Vec 2 (Complex a)
    cexp2 = selectI (SNat @ 0) (SNat @ 2) twiddles

    cexp4 :: Vec 4 (Complex a)
    cexp4 = selectI (SNat @ 0) (SNat @ 1) twiddles

    fft1 :: Vec 1 (Complex a) -> Vec 1 (Complex a)
    fft1 = id

    fft2 :: Vec 2 (Complex a) -> Vec 2 (Complex a)
    fft2 = fftStepDIFRec cexp1 fft1

    fft4 :: Vec 4 (Complex a) -> Vec 4 (Complex a)
    fft4 = fftStepDIFRec cexp2 fft2

    fft8 :: Vec 8 (Complex a) -> Vec 8 (Complex a)
    fft8 = fftStepDIFRec cexp4 fft4

fftStepDITIter
    :: forall n a. (Num a, KnownNat n)
    => Vec n (Complex a)
    -> Vec (2 * n) (Complex a)
    -> Vec (2 * n) (Complex a)
fftStepDITIter twiddleFactors input = zipWith (+) (partitioned !! 0) twiddled ++ zipWith (-) (partitioned !! 0) twiddled
    where
    partitioned :: Vec 2 (Vec n (Complex a))
    partitioned =  unconcatI input
    twiddled    =  zipWith (*) twiddleFactors (partitioned !! 1)

fftDITIter 
    :: forall a. (Num a)
    => Vec 4 (Complex a)
    -> Vec 8 (Complex a)
    -> Vec 8 (Complex a)
fftDITIter twiddles (a :> b :> c :> d :> e :> f :> g :> h :> Nil) = fft8
    where

    reorderedInput = a :> e :> c :> g :> b :> f :> d :> h :> Nil

    cexp1 :: Vec 1 (Complex a)
    cexp1 = selectI (SNat @ 0) (SNat @ 4) twiddles

    cexp2 :: Vec 2 (Complex a)
    cexp2 = selectI (SNat @ 0) (SNat @ 2) twiddles

    cexp4 :: Vec 4 (Complex a)
    cexp4 = selectI (SNat @ 0) (SNat @ 1) twiddles

    fft2 = concat $ map (fftStepDITIter cexp1) $ unconcatI reorderedInput
    fft4 = concat $ map (fftStepDITIter cexp2) $ unconcatI fft2
    fft8 = concat $ map (fftStepDITIter cexp4) $ unconcatI fft4

fftStepDIFIter
    :: forall n a. (Num a, KnownNat n)
    => Vec n (Complex a)
    -> Vec (2 * n) (Complex a)
    -> Vec (2 * n) (Complex a)
fftStepDIFIter twiddleFactors input = butterflied1 ++ twiddled 
    where
    partitioned  :: Vec 2 (Vec n (Complex a))
    partitioned  =  unconcatI input
    butterflied1 =  zipWith (+) (partitioned !! 0) (partitioned !! 1)
    butterflied2 =  zipWith (-) (partitioned !! 0) (partitioned !! 1)
    twiddled     =  zipWith (*) twiddleFactors butterflied2

fftDIFIter 
    :: forall a. (Num a)
    => Vec 4 (Complex a)
    -> Vec 8 (Complex a)
    -> Vec 8 (Complex a)
fftDIFIter twiddles input = a :> e :> c :> g :> b :> f :> d :> h :> Nil
    where
    
    a :> b :> c :> d :> e :> f :> g :> h :> Nil = fft2

    cexp1 :: Vec 1 (Complex a)
    cexp1 = selectI (SNat @ 0) (SNat @ 4) twiddles

    cexp2 :: Vec 2 (Complex a)
    cexp2 = selectI (SNat @ 0) (SNat @ 2) twiddles

    cexp4 :: Vec 4 (Complex a)
    cexp4 = selectI (SNat @ 0) (SNat @ 1) twiddles

    fft8 = concat $ map (fftStepDIFIter cexp4) $ unconcatI input
    fft4 = concat $ map (fftStepDIFIter cexp2) $ unconcatI fft8
    fft2 = concat $ map (fftStepDIFIter cexp1) $ unconcatI fft4

