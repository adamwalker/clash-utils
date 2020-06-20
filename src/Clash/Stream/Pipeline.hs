-- | Utilities for pipelining streams consisting of data, and valid, ready signals
module Clash.Stream.Pipeline (
        forwardPipeline,
        backwardPipeline
    ) where

import Clash.Prelude

-- | Break combinational paths in the forward (valid and data) direction only
forwardPipeline
    :: forall dom a
    .  (HiddenClockResetEnable dom, NFDataX a)
    => Signal dom Bool
    -> Signal dom a
    -> Signal dom Bool
    -> (Signal dom Bool, Signal dom a, Signal dom Bool)
forwardPipeline vldIn datIn readyIn = (vldOut, datOut, readyOut)
    where

    readyOut :: Signal dom Bool
    readyOut =  readyIn .||. fmap not vldOut

    vldOut :: Signal dom Bool
    vldOut =  register False $ vldIn .||. fmap not readyOut

    datOut :: Signal dom a
    datOut =  regEn (errorX "initial stream forwardPipeline value") readyOut datIn

-- | Break combinational paths in the backwards (ready) direction only
backwardPipeline
    :: forall dom a
    .  (HiddenClockResetEnable dom, NFDataX a)
    => Signal dom Bool
    -> Signal dom a
    -> Signal dom Bool
    -> (Signal dom Bool, Signal dom a, Signal dom Bool)
backwardPipeline vldIn datIn readyIn = (vldOut, datOut, bufferEmpty)
    where
    bufferEmpty :: Signal dom Bool
    bufferEmpty =  register True $ fmap not vldOut .||. readyIn

    vldOut :: Signal dom Bool
    vldOut =  fmap not bufferEmpty .||. vldIn

    datSaved :: Signal dom a
    datSaved =  regEn (errorX "initial stream backwardPipeline value") bufferEmpty datIn

    datOut :: Signal dom a
    datOut =  mux bufferEmpty datIn datSaved 

