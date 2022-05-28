module Clash.DSP.FIR.HalfBand (
        delayLine,
        halfBandDecimate
    ) where

import Clash.Prelude

import Clash.Counter

delayLine 
    :: forall dom delay a
    .  (HiddenClockResetEnable dom, Num a, 1 <= delay)
    => KnownNat delay
    => NFDataX a
    => SNat delay
    -> Signal dom Bool
    -> Signal dom a
    -> (Signal dom a)
delayLine delay valid sampleIn = readResD
    where

    readAddr, writeAddr :: Signal dom (Unsigned (CLog 2 delay))
    readAddr  = count 0 valid
    writeAddr = count (snatToNum delay) valid

    readRes, readResD :: Signal dom a
    readRes  = blockRamPow2 (repeat 0) readAddr (mux valid (Just <$> bundle (writeAddr, sampleIn)) (pure Nothing))
    readResD = regEn (errorX "initial readResD") valid readRes

type FilterBP dom inputType outputType
    =  Signal dom outputType    -- ^ Cascade
    -> Signal dom Bool          -- ^ Input valid
    -> Signal dom inputType     -- ^ Sample
    -> (
        Signal dom Bool, 
        Signal dom outputType, 
        Signal dom Bool
        )                       -- ^ (Output valid, output data, ready)

halfBandDecimate
    :: forall dom delay inputType outputType
    .  HiddenClockResetEnable dom
    => (Num inputType, NFDataX inputType)
    => (1 <= delay, KnownNat delay)
    => SNat delay
    -> (inputType -> outputType)
    -> FilterBP dom inputType outputType
    -> Signal dom Bool
    -> Signal dom inputType
    -> (
        Signal dom Bool, 
        Signal dom outputType, 
        Signal dom Bool
        )
halfBandDecimate delay convert filter valid sampleIn = (hValid, hOut, ready)
    where

    --Sequencing
    phase :: Signal dom Bool
    phase =  regEn False (ready .&&. valid) (not <$> phase)

    ready :: Signal dom Bool
    ready 
        =    phase
        .||. hReady

    --instantiate the filter
    (hValid, hOut, hReady) = filter (convert <$> cascade) (not <$> phase .&&. valid) sampleIn

    --instantiate the delay line
    delayOut, cascade :: Signal dom inputType
    delayOut = delayLine delay (valid .&&. phase) sampleIn

    cascade = mux hReady delayOut 0

