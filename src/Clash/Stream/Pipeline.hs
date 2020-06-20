-- | Utilities for pipelining streams consisting of data, and valid, ready signals
module Clash.Stream.Pipeline (
        forwardPipeline,
        backwardPipeline,
        skidBufferInReg,
        skidBufferOutReg
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
    bufferEmpty =  register True $ readyIn .||. fmap not vldOut 

    vldOut :: Signal dom Bool
    vldOut =  vldIn .||. fmap not bufferEmpty

    datSaved :: Signal dom a
    datSaved =  regEn (errorX "initial stream backwardPipeline value") bufferEmpty datIn

    datOut :: Signal dom a
    datOut =  mux bufferEmpty datIn datSaved 

-- | Break combinational paths in both the forward and reverse directions. Inputs are registered. https://zipcpu.com/blog/2019/05/22/skidbuffer.html.
skidBufferInReg
    :: forall dom a
    .  (HiddenClockResetEnable dom, NFDataX a)
    => Signal dom Bool
    -> Signal dom a
    -> Signal dom Bool
    -> (Signal dom Bool, Signal dom a, Signal dom Bool)
skidBufferInReg vldIn datIn readyIn = (p2Vld, p2Dat, p1Rdy)
    where
    (p1Vld, p1Dat, p1Rdy) = forwardPipeline  vldIn datIn p2Rdy
    (p2Vld, p2Dat, p2Rdy) = backwardPipeline p1Vld p1Dat readyIn

-- | Break combinational paths in both the forward and reverse directions. Outputs are registered. https://zipcpu.com/blog/2019/05/22/skidbuffer.html.
skidBufferOutReg
    :: forall dom a
    .  (HiddenClockResetEnable dom, NFDataX a)
    => Signal dom Bool
    -> Signal dom a
    -> Signal dom Bool
    -> (Signal dom Bool, Signal dom a, Signal dom Bool)
skidBufferOutReg vldIn datIn readyIn = (p2Vld, p2Dat, p1Rdy)
    where
    (p1Vld, p1Dat, p1Rdy) = backwardPipeline vldIn datIn p2Rdy
    (p2Vld, p2Dat, p2Rdy) = forwardPipeline  p1Vld p1Dat readyIn

