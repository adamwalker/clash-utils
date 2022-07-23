{-| 
    Half band decimators.

    __FPGA proven__
-}
module Clash.DSP.FIR.HalfBand (
        delayLine,
        halfBandDecimate',
        halfBandDecimate
    ) where

import Clash.Prelude

import Clash.Counter
import Clash.DSP.MAC(MACPreAdd)
import Clash.DSP.FIR.SemiParallel(evenSymmAccum2, semiParallelFIRSystolicSymmetric)

delayLine 
    :: forall dom delayN a
    .  (HiddenClockResetEnable dom, Num a, 1 <= delayN)
    => KnownNat delayN
    => NFDataX a
    => SNat delayN
    -> Signal dom Bool
    -> Signal dom a
    -> Signal dom a
delayLine delayN valid sampleIn = readResD
    where

    readAddr, writeAddr :: Signal dom (Unsigned (CLog 2 delayN))
    readAddr  = count 0 valid
    writeAddr = count (snatToNum delayN) valid

    readRes, readResD :: Signal dom a
    readRes  = blockRamPow2 (repeat 0) readAddr (mux valid (Just <$> bundle (writeAddr, sampleIn)) (pure Nothing))
    readResD = delayEn (errorX "initial readResD") valid readRes

type FilterBP dom inputType outputType
    =  Signal dom outputType    -- ^ Cascade
    -> Signal dom Bool          -- ^ Input valid
    -> Signal dom inputType     -- ^ Sample
    -> (
        Signal dom Bool, 
        Signal dom outputType, 
        Signal dom Bool
        )                       -- ^ (Output valid, output data, ready)

halfBandDecimate'
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
halfBandDecimate' delay convert filter valid sampleIn = (hValid, hOut, ready)
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

halfBandDecimate 
    :: forall dom macDelay delayLineDelay coeffType inputType outputType numStages numCoeffs 
    .  HiddenClockResetEnable dom
    => ((delayLineDelay + 1) ~ ((numStages + 1) * numCoeffs), 1 <= delayLineDelay, KnownNat delayLineDelay)
    => (KnownNat numCoeffs, 1 <= numCoeffs)
    => KnownNat numStages
    => (Num inputType, NFDataX inputType)
    => (Num outputType, NFDataX outputType)
    => (Num coeffType, NFDataX coeffType)
    => MACPreAdd dom coeffType inputType outputType
    -> SNat macDelay
    -> (inputType -> outputType)
    -> Vec (numStages + 1) (Vec numCoeffs coeffType)
    -> Signal dom Bool
    -> Signal dom inputType
    -> (Signal dom Bool, Signal dom outputType, Signal dom Bool)
halfBandDecimate mac macDelay convert coeffs = halfBandDecimate' (SNat @delayLineDelay) convert filter
    where
    filter 
        :: Signal dom outputType 
        -> Signal dom Bool 
        -> Signal dom inputType 
        -> (Signal dom Bool, Signal dom outputType, Signal dom Bool)
    filter 
        = semiParallelFIRSystolicSymmetric 
            mac 
            (evenSymmAccum2 macDelay mac (last coeffs)) 
            macDelay 
            (init coeffs)

