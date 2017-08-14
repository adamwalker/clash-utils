{-# LANGUAGE ScopedTypeVariables #-}

{-| FIR filters: see <http://adamwalker.github.io/Filter-Design-in-Clash/> -}
module CLaSH.FIRFilter (
    fir,
    firTransposed,
    firSystolic,
    firSymmetric,
    firTransposedSymmetric,
    firSystolicSymmetric,
    firSystolicSymmetricOdd,
    firSystolicHalfBand
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

