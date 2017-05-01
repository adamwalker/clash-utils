{-# LANGUAGE ScopedTypeVariables, GADTs #-}

module CLaSH.FFT (
    fft
    ) where

import CLaSH.Prelude

import CLaSH.Complex

fftStep :: forall n a . (Num a, KnownNat n)
        => Vec n (Complex a)
        -> (Vec n (Complex a) -> Vec n (Complex a))
        -> Vec (2 * n) (Complex a)
        -> Vec (2 * n) (Complex a)
fftStep twiddleFactors recurse input = zipWith (+) fft1 twiddled ++ zipWith (-) fft1 twiddled
    where
    grouped     :: Vec n (Vec 2 (Complex a))
    grouped     =  unconcatI input
    partitioned :: Vec 2 (Vec n (Complex a))
    partitioned =  transpose grouped

    fft1        =  recurse $ partitioned !! 0
    fft2        =  recurse $ partitioned !! 1
    twiddled    =  zipWith (*) twiddleFactors fft2

--TODO: use dependently typed fold to automate all this
--A completely impractical purely combinational FFT
fft :: forall a. (Num a, Floating a)
    => Vec 8 (Complex a) 
    -> Vec 8 (Complex a)
fft = fft8 
    where
    cexp1 :: Vec 1 (Complex a)
    cexp1 = (1 :+ 0) :> Nil

    cexp2 :: Vec 2 (Complex a)
    cexp2 = (1 :+ 0) :>  (0 :+ (-1)) :> Nil

    invRoot2 = sqrt 2 / 2

    --TODO: require complex exp constant sequence as an argument
    cexp4 :: Vec 4 (Complex a)
    cexp4 = iterateI (* (invRoot2 :+ (- invRoot2))) 1

    fft1 :: Vec 1 (Complex a) -> Vec 1 (Complex a)
    fft1 = id

    fft2 :: Vec 2 (Complex a) -> Vec 2 (Complex a)
    fft2 = fftStep cexp1 fft1

    fft4 :: Vec 4 (Complex a) -> Vec 4 (Complex a)
    fft4 = fftStep cexp2 fft2

    fft8 :: Vec 8 (Complex a) -> Vec 8 (Complex a)
    fft8 = fftStep cexp4 fft4

