module Clash.DSP.FFT.Butterfly (
        Butterfly,
        difButterfly,
        ditButterfly
    ) where

import Clash.Prelude

type Butterfly dom twiddle input output 
    =  Signal dom twiddle
    -> Signal dom input
    -> Signal dom input
    -> (Signal dom output, Signal dom output)

difButterfly 
    :: Num a
    => Butterfly dom a a a
difButterfly twiddle upper lower = (sum, twiddle * diff)
    where
    sum  = upper + lower
    diff = upper - lower

ditButterfly 
    :: Num a
    => Butterfly dom a a a
ditButterfly twiddle upper lower = (sum, diff)
    where
    twiddled = lower * twiddle
    sum      = upper + twiddled
    diff     = upper - twiddled
