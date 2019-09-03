{-| 
    CIC decimator: https://en.wikipedia.org/wiki/Cascaded_integrator%E2%80%93comb_filter
-}
module Clash.DSP.CICFilter (
    integrate,
    comb,
    nth,
    cicDecimate
    ) where

import Clash.Prelude

integrate
    :: forall dom a
    . (HiddenClockResetEnable dom, Undefined a, Num a)
    => Signal dom Bool
    -> Signal dom a
    -> Signal dom a
integrate valid x = res
    where
    res = regEn 0 valid $ res + x

comb 
    :: forall dom m a
    .  (HiddenClockResetEnable dom, KnownNat m, Undefined a, Num a)
    => SNat m 
    -> Signal dom Bool
    -> Signal dom a
    -> Signal dom a
comb SNat valid x = x - last delayLine
    where
    delayLine :: Vec (m + 1) (Signal dom a)
    delayLine =  iterateI (regEn 0 valid) x

nth 
    :: forall dom d
    .  HiddenClockResetEnable dom
    => SNat d
    -> Signal dom Bool
    -> Signal dom Bool
nth SNat valid = (cnt .==. 0) .&&. valid
    where
    cnt :: Signal dom (Index d)
    cnt = regEn 0 valid $ func <$> cnt
        where
        func cnt
            | cnt == maxBound = 0
            | otherwise       = cnt + 1

cicDecimate
    :: forall dom m r n a
    .  (HiddenClockResetEnable dom, KnownNat m, Undefined a, Num a)
    => SNat r
    -> SNat m
    -> SNat n
    -> Signal dom Bool
    -> Signal dom a
    -> (Signal dom Bool, Signal dom a)
cicDecimate r m SNat valid x = (validN, result)
    where
    integrated = last $ iterate (SNat @ (n + 1)) (integrate valid) x
    validN     = nth r valid
    result     = last $ iterate (SNat @ (n + 1)) (regEn 0 validN . comb m validN) integrated

