--Infinite impulse response filters
module CLaSH.IIRFilter (
    iirDirectI,
    iirDirectII,
    iirTransposedI,
    iirTransposedII
    ) where

import CLaSH.Prelude

{- | Direct form I: <https://www.dsprelated.com/freebooks/filters/Direct_Form_I.html> -}
iirDirectI
    :: (Num a, KnownNat n)
    => Vec (n + 2) a -- ^ Numerator coefficients
    -> Vec (n + 1) a -- ^ Denominator coefficients
    -> Signal Bool   -- ^ Input enable
    -> Signal a      -- ^ Input sample
    -> Signal a      -- ^ Output sample
iirDirectI coeffsN coeffsD en x = res
    where
    res        = fir + iir
    fir        = dotP (map pure coeffsN) (iterateI (regEn 0 en) x)
    iir        = dotP (map pure coeffsD) (iterateI (regEn 0 en) (regEn 0 en res))
    dotP as bs = fold (+) (zipWith (*) as bs)

{- | Direct form II: <https://www.dsprelated.com/freebooks/filters/Direct_Form_II.html> -}
iirDirectII
    :: (Num a, KnownNat n)
    => Vec (n + 2) a -- ^ Numerator coefficients
    -> Vec (n + 1) a -- ^ Denominator coefficients
    -> Signal Bool   -- ^ Input enable
    -> Signal a      -- ^ Input sample
    -> Signal a      -- ^ Output sample
iirDirectII coeffsN coeffsD en x = dotP (map pure coeffsN) delayed
    where
    delayed    = iterateI (regEn 0 en) mid 
    mid        = x + dotP (map pure coeffsD) (tail delayed)  
    dotP as bs = fold (+) (zipWith (*) as bs)

{- | Transposed form I: <https://www.dsprelated.com/freebooks/filters/Transposed_Direct_Forms.html> -}
iirTransposedI
    :: (Num a, KnownNat n)
    => Vec (n + 2) a -- ^ Numerator coefficients
    -> Vec (n + 1) a -- ^ Denominator coefficients
    -> Signal Bool   -- ^ Input enable
    -> Signal a      -- ^ Input sample
    -> Signal a      -- ^ Output sample
iirTransposedI coeffsN coeffsD en x = foldl1 func $ reverse $ map (* v) (pure <$> coeffsN)
    where
    v            = x + regEn 0 en (foldl1 func $ reverse $ map (* v) (pure <$> coeffsD))
    func accum x = x + regEn 0 en accum 

{- | Transposed form II: <https://www.dsprelated.com/freebooks/filters/Transposed_Direct_Forms.html> -}
iirTransposedII
    :: (Num a, KnownNat n)
    => Vec (n + 2) a -- ^ Numerator coefficients
    -> Vec (n + 1) a -- ^ Denominator coefficients
    -> Signal Bool   -- ^ Input enable
    -> Signal a      -- ^ Input sample
    -> Signal a      -- ^ Output sample
iirTransposedII coeffsN coeffsD en x = res
    where
    res = head fir + regEn 0 en t 
    fir = map (* x)   (pure <$> coeffsN)
    iir = map (* res) (pure <$> coeffsD)
    ts  = reverse $ zipWith (+) (tail fir) iir
    t   = foldl1 (\accum inp -> regEn 0 en accum + inp) ts

