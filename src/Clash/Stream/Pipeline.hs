module Clash.Stream.Pipeline (
        pipeline
    ) where

import Clash.Prelude

pipeline
    :: forall dom a
    .  (HiddenClockResetEnable dom, NFDataX a)
    => Signal dom Bool
    -> Signal dom a
    -> Signal dom Bool
    -> (Signal dom Bool, Signal dom a, Signal dom Bool)
pipeline vldIn datIn readyIn = (vldOut, datOut, readyOut)
    where

    readyOut :: Signal dom Bool
    readyOut =  readyIn .||. fmap not vldOut

    vldOut :: Signal dom Bool
    vldOut =  register False $ vldIn .||. (vldOut .&&. fmap not readyIn)

    datOut :: Signal dom a
    datOut =  regEn (errorX "initial stream pipeline value") readyOut datIn
