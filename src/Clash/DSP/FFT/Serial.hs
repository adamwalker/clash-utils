{-| Radix 2 complex-to-complex Cooley-Tukey FFTs. https://en.wikipedia.org/wiki/Cooley%E2%80%93Tukey_FFT_algorithm.
    The FFTs in this module are pipelined, saving multiplers and routing resources. They operate on and produce two complex numbers at a time. 
-}
module Clash.DSP.FFT.Serial (
    fftBase,
    fftSerialDITStep,
    fftSerialDIFStep
    ) where

import Clash.Prelude

import Clash.Counter(count)
import Clash.DSP.Complex
import Clash.DSP.FFT.Butterfly

fftBase 
    :: (HiddenClockResetEnable dom, Num a, NFDataX a) 
    => Signal dom Bool 
    -> Signal dom (Complex a, Complex a) 
    -> Signal dom (Complex a, Complex a)
fftBase en = delayEn (errorX "initial fftBase", errorX "initial fftBase") en . fmap func
    where
    func (x, y) = (x + y, x - y)

{-
 -                                 |\     _______
 - Upper in -----------------------|0|   |       |
 -                        \        | |---|  RAM  |--- Upper out
 -                         \    ---|1|   |>______|
 -                          \  /   |/
 -                           \/
 -                           /\
 -                          /  \   |\
 -              _______    /    ---|1|
 -             |       |  /        | |--------------- Lower out
 - Lower in ---|  RAM  |-----------|0|
 -             |>______|           |/
 -}
fftReorder
    :: forall dom n a. (HiddenClockResetEnable dom, KnownNat n, NFDataX a)
    => Signal dom Bool
    -> Signal dom Bool
    -> Signal dom (Unsigned n)
    -> Signal dom a
    -> Signal dom a
    -> (Signal dom a, Signal dom a)
fftReorder en stage address upper lower = (upperRamReadResult, lowerData)
    where

    upperD = delayEn (errorX "initial upperD") en upper

    --Swap
    upperData = mux (not <$> delayEn (errorX "initial upperData sel") en stage) upperD             lowerRamReadResult
    lowerData = mux (not <$> delayEn (errorX "initial lowerData sel") en stage) lowerRamReadResult upperD

    --Buffer
    lowerRamReadResult = blockRamPow2 (repeat (errorX "initial FFT RAM") :: Vec (2 ^ n) a) address 
        $ mux en (Just <$> bundle (address, lower)) (pure Nothing)
    upperRamReadResult = blockRamPow2 (repeat (errorX "initial FFT RAM") :: Vec (2 ^ n) a) (delayEn (errorX "initial upper read address") en address)
        $ mux en (Just <$> bundle (delayEn (errorX "initial upper write address") en address, upperData)) (pure Nothing)

--Decimation in time
--2^(n + 1) == size of FFT / 2 == number of butterfly input pairs
-- | A step in the serial FFT decimation in time algorithm. Consumes and produces two complex samples per cycle. 
fftSerialDITStep
    :: forall dom n twiddle input output. (HiddenClockResetEnable dom, KnownNat n, NFDataX input)
    => Butterfly dom twiddle input output
    -> Vec (2 ^ (n + 1)) twiddle          -- ^ Precomputed twiddle factors
    -> Signal dom Bool                    -- ^ Input enable signal
    -> Signal dom (input,  input)         -- ^ Pair of input samples
    -> Signal dom (output, output)        -- ^ Pair of output samples
fftSerialDITStep butterfly twiddles en input = bundle (butterflyHighOutput, butterflyLowOutput)
    where

    --The state
    counter :: Signal dom (BitVector (n + 1))
    counter =  count 0 en

    (stage :: Signal dom Bool, address :: Signal dom (Unsigned n)) 
        = unbundle $ bitCoerce <$> counter

    --The FIFOs
    (upperRamReadResult, lowerData) = fftReorder en stage address (fst <$> input) (snd <$> input)

    --The butterfly
    twiddlesRot = rotateLeftS twiddles (SNat @(2 ^ n))
    twiddle     = (twiddlesRot !!) <$> delayEn (errorX "initial twiddle") en (delayEn (errorX "initial twiddle") en counter)

    (butterflyHighOutput, butterflyLowOutput) 
        = butterfly twiddle upperRamReadResult (delayEn (errorX "initial butterfly input") en lowerData)

--Decimation in frequency
--2^(n + 1) == size of FFT / 2 == number of butterfly input pairs
-- | A step in the serial FFT decimation in frequency algorithm. Consumes and produces two complex samples per cycle. 
fftSerialDIFStep
    :: forall dom n twiddle input output. (HiddenClockResetEnable dom, KnownNat n, NFDataX output)
    => Butterfly dom twiddle input output
    -> Vec (2 ^ (n + 1)) twiddle          -- ^ Precomputed twiddle factors
    -> Signal dom Bool                    -- ^ Input enable signal
    -> Signal dom (input, input)          -- ^ Pair of input samples
    -> Signal dom (output, output)        -- ^ Pair of output samples
fftSerialDIFStep butterfly twiddles en input = bundle (upperRamReadResult, delayEn (errorX "initial FFT out") en lowerData)
    where

    --The state
    counter :: Signal dom (BitVector (n + 1))
    counter =  count 0 en

    (stage :: Signal dom Bool, address :: Signal dom (Unsigned n)) 
        = unbundle $ bitCoerce <$> counter

    --The butterfly
    twiddle = (twiddles !!) <$> counter

    (butterflyHighOutput, butterflyLowOutput) 
        = butterfly twiddle (fmap fst input) (fmap snd input)

    --The FIFOs
    (upperRamReadResult, lowerData) = fftReorder en stage address butterflyHighOutput butterflyLowOutput

