{-# LANGUAGE ScopedTypeVariables, GADTs #-}

module CLaSH.FFT (
    twiddleFactors,
    fftDIT,
    fftDIF
    ) where

import CLaSH.Prelude

import CLaSH.Complex
import qualified Data.Complex as C
import qualified Prelude as P

twiddleFactors :: Int -> [Complex Double]
twiddleFactors num = P.take num $ [fromComplex $ C.cis $ (-1) * P.pi * fromIntegral i / (fromIntegral num) | i <- [0..]]

fftStepDIT :: forall n a . (Num a, KnownNat n)
        => Vec n (Complex a)
        -> (Vec n (Complex a) -> Vec n (Complex a))
        -> Vec (2 * n) (Complex a)
        -> Vec (2 * n) (Complex a)
fftStepDIT twiddleFactors recurse input = zipWith (+) fft1 twiddled ++ zipWith (-) fft1 twiddled
    where
    grouped     :: Vec n (Vec 2 (Complex a))
    grouped     =  unconcatI input
    partitioned :: Vec 2 (Vec n (Complex a))
    partitioned =  transpose grouped

    fft1        =  recurse $ partitioned !! 0
    fft2        =  recurse $ partitioned !! 1
    twiddled    =  zipWith (*) twiddleFactors fft2

fftStepDIF :: forall n a . (Num a, KnownNat n)
           => Vec n (Complex a)
           -> (Vec n (Complex a) -> Vec n (Complex a))
           -> Vec (2 * n) (Complex a)
           -> Vec (2 * n) (Complex a)
fftStepDIF twiddleFactors recurse input = fft1 ++ fft2
    where
    partitioned :: Vec 2 (Vec n (Complex a))
    partitioned =  unconcatI input

    fft1        = recurse $ zipWith (+) (partitioned !! 0) (partitioned !! 1)
    fft2        = recurse $ zipWith (*) twiddleFactors $ zipWith (-) (partitioned !! 0) (partitioned !! 1)

--TODO: use dependently typed fold to automate all this
--A completely impractical purely combinational FFT
fftDIT :: forall a. (Num a, Floating a)
    => Vec 4 (Complex a)
    -> Vec 8 (Complex a) 
    -> Vec 8 (Complex a)
fftDIT twiddles = fft8 
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
    fft2 = fftStepDIT cexp1 fft1

    fft4 :: Vec 4 (Complex a) -> Vec 4 (Complex a)
    fft4 = fftStepDIT cexp2 fft2

    fft8 :: Vec 8 (Complex a) -> Vec 8 (Complex a)
    fft8 = fftStepDIT cexp4 fft4

--TODO: use dependently typed fold to automate all this
--A completely impractical purely combinational FFT
fftDIF :: forall a. (Num a, Floating a)
    => Vec 4 (Complex a)
    -> Vec 8 (Complex a) 
    -> Vec 8 (Complex a)
fftDIF twiddles input = (a :> e :> c :> g :> b :> f :> d :> h :> Nil)
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
    fft2 = fftStepDIF cexp1 fft1

    fft4 :: Vec 4 (Complex a) -> Vec 4 (Complex a)
    fft4 = fftStepDIF cexp2 fft2

    fft8 :: Vec 8 (Complex a) -> Vec 8 (Complex a)
    fft8 = fftStepDIF cexp4 fft4

