{-# LANGUAGE ScopedTypeVariables #-}

{-| FIR filters: see <http://adamwalker.github.io/Filter-Design-in-Clash/> -}
{-| These are based on designs in the Xilinx document <http://www-inst.eecs.berkeley.edu/~cs150/fa13/resources/dsp.pdf DSP: Designing for Optimal Results> -}
module CLaSH.FIRFilter (
    fir,
    firTransposed,
    firSystolic,
    firSymmetric,
    firTransposedSymmetric,
    firSystolicSymmetric,
    firSystolicSymmetricOdd,
    firSystolicHalfBand,
    semiParallelFIR
    ) where

import CLaSH.Prelude

{- | Direct form FIR filter -}
fir 
    :: (Num a, KnownNat n) 
    => Vec (n + 1) a -- ^ Coefficients
    -> Signal Bool   -- ^ Input enable
    -> Signal a      -- ^ Input samples
    -> Signal a      -- ^ Output samples
fir coeffs en x = dotp (map pure coeffs) (iterateI (regEn 0 en) x)
    where
    dotp as bs = fold (+) (zipWith (*) as bs)

{- | Transposed FIR filter -}
firTransposed 
    :: (Num a, KnownNat n) 
    => Vec (n + 1) a -- ^ Coefficients
    -> Signal Bool   -- ^ Input enable
    -> Signal a      -- ^ Input samples
    -> Signal a      -- ^ Output samples
firTransposed coeffs en x = foldl func 0 $ map (* x) (pure <$> coeffs)
    where
    func accum x = regEn 0 en $ accum + x

{- | Systolic FIR filter -}
firSystolic 
    :: (Num a, KnownNat n) 
    => Vec (n + 1) a -- ^ Coefficients
    -> Signal Bool   -- ^ Input enable
    -> Signal a      -- ^ Input samples
    -> Signal a      -- ^ Output samples
firSystolic coeffs en x = foldl func 0 $ zip (map pure coeffs) $ iterateI (regEn 0 en . regEn 0 en) x
    where
    func accum (coeff, input) = regEn 0 en $ accum + input * coeff

{- | Symmetric FIR filter -}
firSymmetric
    :: (KnownNat n, Num a) 
    => Vec (n + 1) a -- ^ Coefficients
    -> Signal Bool   -- ^ Input enable
    -> Signal a      -- ^ Input samples
    -> Signal a      -- ^ Output samples
firSymmetric coeffs en x = foldl func 0 $ zipWith (*) folded (pure <$> coeffs)
    where
    func accum x = regEn 0 en $ accum + x
    folded       = map (+ x) delayed
    delayed      = iterate (lengthS coeffs) (regEn 0 en . regEn 0 en) (regEn 0 en x)

{- | Transposed symmetric FIR filter -}
firTransposedSymmetric
    :: (Num a, KnownNat n) 
    => Vec (n + 1) a -- ^ Coefficients
    -> Signal Bool   -- ^ Input enable
    -> Signal a      -- ^ Input samples
    -> Signal a      -- ^ Output samples
firTransposedSymmetric coeffs en x = foldl func 0 $ coeffd ++ reverse coeffd
    where
    coeffd       = map (* x) (pure <$> coeffs)
    func accum x = regEn 0 en $ accum + x

{- | Systolic Symmetric FIR filter -}
firSystolicSymmetric
    :: (KnownNat n, Num a) 
    => Vec (n + 1) a -- ^ Coefficients
    -> Signal Bool   -- ^ Input enable
    -> Signal a      -- ^ Input samples
    -> Signal a      -- ^ Output samples
firSystolicSymmetric coeffs en x = foldl func 0 $ zip (map pure coeffs) folded
    where
    delayLine                 = iterateI (regEn 0 en . regEn 0 en) x
    lastDelayLine             = regEn 0 en $ last delayLine
    folded                    = map (+ lastDelayLine) delayLine
    func accum (coeff, input) = regEn 0 en $ accum + input * coeff
    
firSystolicSymmetricOdd
    :: (KnownNat n, Num a) 
    => Vec (n + 2) a -- ^ Coefficients
    -> Signal Bool   -- ^ Input enable
    -> Signal a      -- ^ Input samples
    -> Signal a      -- ^ Output samples
firSystolicSymmetricOdd coeffs en x = foldl func 0 $ zip (map pure coeffs) folded
    where
    delayLine                 = iterateI (regEn 0 en . regEn 0 en) x
    lastDelayLine             = regEn 0 en $ regEn 0 en $ last delayLine
    folded                    = map (+ lastDelayLine) delayLine ++ singleton lastDelayLine
    func accum (coeff, input) = regEn 0 en $ accum + input * coeff
    
firSystolicHalfBand
    :: (KnownNat n, Num a) 
    => Vec (n + 2) a -- ^ Coefficients
    -> Signal Bool   -- ^ Input enable
    -> Signal a      -- ^ Input samples
    -> Signal a      -- ^ Output samples
firSystolicHalfBand coeffs en x = foldl func 0 $ zip (map pure coeffs) folded
    where
    delayLine                 = iterateI (regEn 0 en . regEn 0 en . regEn 0 en) x
    lastDelayLine             = regEn 0 en $ regEn 0 en $ last delayLine
    delayedReturn             = iterateI (regEn 0 en) lastDelayLine
    folded                    = zipWith (+) delayLine (reverse delayedReturn) ++ singleton lastDelayLine
    func accum (coeff, input) = regEn 0 en $ accum + input * coeff

semiParallelFIR 
    :: forall a n m n' m'. (Num a, KnownNat n, KnownNat m, n ~ (n' + 1), m ~ (m' + 1))
    => Vec n (Vec m a)
    -> Signal Bool
    -> Signal a
    -- -> Signal (Bool, Vec n (Vec m a), Vec n (Index m), Vec n a, a, Bool, a)
    -> Signal a
semiParallelFIR coeffs en x = accum
--semiParallelFIR coeffs en x = bundle $ (stepChunk, sequenceA inputChunks, sequenceA addresses, sequenceA currentSamples, outputStream, dumpIt, accum)
    where

    --Hopefully this will be implemented in SRL16s
    inputChunks :: Vec n (Signal (Vec m a))
    inputChunks = zipWith (regEn (repeat 0)) stepChunks $ zipWith (liftA2 (+>>)) lastInChunk inputChunks 

    lastInChunk :: Vec n (Signal a)
    lastInChunk = x +>> currentSamples 

    address :: Signal (Index m)
    address = regEn 0 en (wrappingInc <$> address)
        where
        wrappingInc x
            | x == maxBound = 0
            | otherwise     = x + 1

    stepChunk :: Signal Bool
    stepChunk = (address .==. 0) .&&. en

    stepChunks :: Vec n (Signal Bool)
    stepChunks = iterateI (regEn False en) stepChunk

    addresses :: Vec n (Signal (Index m))
    addresses = tail $ iterateI (regEn 0 en) address

    currentSamples :: Vec n (Signal a)
    currentSamples = map (regEn 0 en) $ zipWith (liftA2 (!!)) inputChunks addresses

    currentCoefficients :: Vec n (Signal a)
    currentCoefficients = map (regEn 0 en) $ zipWith func coeffs addresses
        where
        func coeffs idx = (coeffs !!) <$> idx

    multiplied :: Vec n (Signal a)
    multiplied = map (regEn 0 en) $ zipWith (*) currentCoefficients currentSamples

    outputStream :: Signal a
    outputStream = foldl func 0 multiplied
        where
        func accum x = regEn 0 en $ accum + x

    dumpIt :: Signal Bool
    dumpIt = last $ iterate (SNat @ (4 + n + m)) (regEn False en) stepChunk

    accum :: Signal a
    accum = regEn 0 en (mux dumpIt (pure 0) accum + outputStream)

