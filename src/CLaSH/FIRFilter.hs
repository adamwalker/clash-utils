{-# LANGUAGE ScopedTypeVariables #-}
module CLaSH.FIRFilter where

import CLaSH.Prelude

fir :: (Num a, KnownNat (n + 1), KnownNat n) => Vec (n + 1) a -> Signal a -> Signal a
fir coeffs x = dotp (map pure coeffs) (iterateI (register 0) x)
    where
    dotp as bs = fold (+) (zipWith (*) as bs)

firTransposed :: (Num a, KnownNat (n + 1)) => Vec (n + 1) a -> Signal a -> Signal a
firTransposed coeffs x = foldl func 0 $ map (* x) (pure <$> coeffs)
    where
    func accum x = register 0 $ accum + x

firLinearPhase :: (KnownNat (n + 1), Num a) => Vec (n + 1) a -> Signal a -> Signal a
firLinearPhase coeffs x = foldl func 0 $ zipWith (*) folded (pure <$> coeffs)
    where
    func accum x = register 0 $ accum + x
    folded       = map (+ x) delayed
    delayed      = iterate (lengthS coeffs) (register 0 . register 0) (register 0 x)
