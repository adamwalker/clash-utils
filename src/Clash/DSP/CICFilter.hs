module Clash.DSP.CICFilter (
    integrate,
    comb,
    nth,
    cicDecimate
    ) where

import Clash.Prelude

integrate
    :: forall dom gated sync a
    . (HiddenClockReset dom gated sync, Undefined a, Num a)
    => Signal dom Bool
    -> Signal dom a
    -> Signal dom a
integrate valid x = res
    where
    res = regEn 0 valid $ res + x

comb 
    :: forall dom gated sync m a
    .  (HiddenClockReset dom gated sync, KnownNat m, Undefined a, Num a)
    => SNat m 
    -> Signal dom Bool
    -> Signal dom a
    -> Signal dom a
comb SNat valid x = x - last delayLine
    where
    delayLine :: Vec (m + 1) (Signal dom a)
    delayLine =  iterateI (regEn 0 valid) x

nth 
    :: forall dom gated sync d
    .  HiddenClockReset dom gated sync
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
    :: forall dom gated sync m d order a
    .  (HiddenClockReset dom gated sync, KnownNat m, Undefined a, Num a)
    => SNat d
    -> SNat m
    -> SNat order
    -> Signal dom Bool
    -> Signal dom a
    -> (Signal dom Bool, Signal dom a)
cicDecimate d m SNat valid x = (validN, result)
    where
    integrated = last $ iterate (SNat @ (order + 1)) (integrate valid) x
    validN     = nth d valid
    result     = last $ iterate (SNat @ (order + 1)) (regEn 0 validN . comb m validN) integrated

