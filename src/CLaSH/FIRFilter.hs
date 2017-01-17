{-# LANGUAGE ScopedTypeVariables #-}

{-| FIR filters: see <http://adamwalker.github.io/Filter-Design-in-Clash/> -}
module CLaSH.FIRFilter (
    fir,
    firTransposed,
    firLinearPhase
    ) where

import CLaSH.Prelude

{- | Direct form FIR filter -}
fir 
    :: (Num a, KnownNat (n + 1), KnownNat n) 
    => Vec (n + 1) a -- ^ Coefficients
    -> Signal Bool   -- ^ Input enable
    -> Signal a      -- ^ Input samples
    -> Signal a      -- ^ Output samples
fir coeffs en x = dotp (map pure coeffs) (iterateI (regEn 0 en) x)
    where
    dotp as bs = fold (+) (zipWith (*) as bs)

{- | Transposed FIR filter -}
firTransposed 
    :: (Num a, KnownNat (n + 1)) 
    => Vec (n + 1) a -- ^ Coefficients
    -> Signal Bool   -- ^ Input enable
    -> Signal a      -- ^ Input samples
    -> Signal a      -- ^ Output samples
firTransposed coeffs en x = foldl func 0 $ map (* x) (pure <$> coeffs)
    where
    func accum x = regEn 0 en $ accum + x

{- | Linear phase FIR filter -}
firLinearPhase 
    :: (KnownNat (n + 1), Num a) 
    => Vec (n + 1) a -- ^ Coefficients
    -> Signal Bool   -- ^ Input enable
    -> Signal a      -- ^ Input samples
    -> Signal a      -- ^ Output samples
firLinearPhase coeffs en x = foldl func 0 $ zipWith (*) folded (pure <$> coeffs)
    where
    func accum x = regEn 0 en $ accum + x
    folded       = map (+ x) delayed
    delayed      = iterate (lengthS coeffs) (regEn 0 en . regEn 0 en) (regEn 0 en x)
