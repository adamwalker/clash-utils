module Clash.Counter (
    count,
    upDownCounter
    ) where

import Clash.Prelude

-- | Counts cycles where the input signal is high
count 
    :: (HiddenClockResetEnable dom, Num a, NFDataX a) 
    => Signal dom Bool -- ^ Increment signal
    -> Signal dom a    -- ^ Count output
count inc = res
    where
    res = regEn 0 inc $ res + 1

-- | Counts up and down
upDownCounter 
    :: (HiddenClockResetEnable dom, Num a, NFDataX a)
    => Signal dom Bool -- ^ Increment count
    -> Signal dom Bool -- ^ Decrement count
    -> Signal dom a    -- ^ Count
upDownCounter up down = count
    where
    count = register 0 $ func <$> count <*> up <*> down
        where
        func count True  False = count + 1
        func count False True  = count - 1
        func count _     _     = count
