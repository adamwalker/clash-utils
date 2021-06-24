module Clash.Counter (
    count,
    upDownCounter,
    wrappingCounter
    ) where

import Clash.Prelude

-- | Counts cycles where the input signal is high
count 
    :: (HiddenClockResetEnable dom, Num a, NFDataX a) 
    => a               -- ^ Reset value
    -> Signal dom Bool -- ^ Increment count
    -> Signal dom a    -- ^ Count output
count init inc = res
    where
    res = regEn init inc $ res + 1

-- | Counts up and down
upDownCounter 
    :: (HiddenClockResetEnable dom, Num a, NFDataX a)
    => a               -- ^ Reset value
    -> Signal dom Bool -- ^ Increment count
    -> Signal dom Bool -- ^ Decrement count
    -> Signal dom a    -- ^ Count
upDownCounter init up down = count
    where
    count = register 0 $ func <$> count <*> up <*> down
        where
        func count True  False = count + 1
        func count False True  = count - 1
        func count _     _     = count

-- | Counts up and wraps around at maxBound
wrappingCounter 
    :: (HiddenClockResetEnable dom, Num b, Bounded b, Eq b, NFDataX b)
    => b               -- ^ Reset value
    -> Signal dom Bool -- ^ Increment count
    -> Signal dom b    -- ^ Count output
wrappingCounter init inc = res
    where
    res = regEn init inc (wrappingInc <$> res)
    wrappingInc x
        | x == maxBound = 0
        | otherwise     = x + 1
