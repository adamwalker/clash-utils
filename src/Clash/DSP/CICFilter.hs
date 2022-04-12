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

-- | Running sum
integrate
    :: forall dom a
    . (HiddenClockResetEnable dom, NFDataX a, Num a)
    => Signal dom Bool -- ^ Input valid
    -> Signal dom a    -- ^ Input
    -> Signal dom a    -- ^ Running sum
integrate valid x = res
    where
    res = regEn 0 valid $ res + x

-- | [Comb filter](https://en.wikipedia.org/wiki/Comb_filter)
comb 
    :: forall dom m a
    .  (HiddenClockResetEnable dom, KnownNat m, NFDataX a, Num a)
    => SNat m          -- ^ Delay
    -> Signal dom Bool -- ^ Input valid
    -> Signal dom a    -- ^ Input
    -> Signal dom a    -- ^ Output
comb SNat valid x = x - last delayLine
    where
    delayLine :: Vec (m + 1) (Signal dom a)
    delayLine =  iterateI (regEn 0 valid) x

-- | Generates a pulse every n cycles
nth 
    :: forall dom n
    .  HiddenClockResetEnable dom
    => SNat n          -- ^ N
    -> Signal dom Bool -- ^ Count
    -> Signal dom Bool -- ^ Output pulse
nth SNat valid = (cnt .==. 0) .&&. valid
    where
    cnt :: Signal dom (Index n)
    cnt = regEn 0 valid $ func <$> cnt
        where
        func cnt
            | cnt == maxBound = 0
            | otherwise       = cnt + 1

-- | [CIC decimator](https://en.wikipedia.org/wiki/Cascaded_integrator%E2%80%93comb_filter)
cicDecimate
    :: forall dom m r n a
    .  (HiddenClockResetEnable dom, KnownNat m, NFDataX a, Num a)
    => SNat r                          -- ^ Decimation ratio
    -> SNat m                          -- ^ Nmuber of samples per stage
    -> SNat n                          -- ^ Number of stages
    -> Signal dom Bool                 -- ^ Input valid
    -> Signal dom a                    -- ^ Input
    -> (Signal dom Bool, Signal dom a) -- ^ (Output valid, output)
cicDecimate r m SNat valid x = (validN, result)
    where
    integrated = last $ iterate (SNat @(n + 1)) (integrate valid) x
    validN     = nth r valid
    result     = last $ iterate (SNat @(n + 1)) (regEn 0 validN . comb m validN) integrated

