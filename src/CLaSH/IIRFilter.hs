--Infinite impulse response filters
module CLaSH.IIRFilter where

import CLaSH.Prelude

--Direct form 1
iirDirect1
    :: (Num a, KnownNat n)
    => Vec (n + 2) a -- ^ Numerator coefficients
    -> Vec (n + 1) a -- ^ Denominator coefficients
    -> Signal Bool   -- ^ Input enable
    -> Signal a      -- ^ Input sample
    -> Signal a      -- ^ Output sample
iirDirect1 coeffsN coeffsD en x = res
    where
    res        = fir + iir
    fir        = dotP (map pure coeffsN) (iterateI (regEn 0 en) x)
    iir        = dotP (map pure coeffsD) (iterateI (regEn 0 en) (regEn 0 en res))
    dotP as bs = fold (+) (zipWith (*) as bs)

--Direct form 2
iirDirect2
    :: (Num a, KnownNat n)
    => Vec (n + 2) a -- ^ Numerator coefficients
    -> Vec (n + 1) a -- ^ Denominator coefficients
    -> Signal Bool   -- ^ Input enable
    -> Signal a      -- ^ Input sample
    -> Signal a      -- ^ Output sample
iirDirect2 coeffsN coeffsD en x = dotP (map pure coeffsN) delayed
    where
    delayed    = iterateI (regEn 0 en) mid 
    mid        = x + dotP (map pure coeffsD) (tail delayed)  
    dotP as bs = fold (+) (zipWith (*) as bs)
