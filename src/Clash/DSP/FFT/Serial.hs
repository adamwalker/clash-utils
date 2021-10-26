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
fftBase en = regEn (0, 0) en . fmap func
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
    :: forall dom n a. (HiddenClockResetEnable dom, KnownNat n, Num a, NFDataX a)
    => Signal dom Bool
    -> Signal dom Bool
    -> Signal dom (Unsigned n)
    -> Signal dom a
    -> Signal dom a
    -> (Signal dom a, Signal dom a)
fftReorder en stage address upper lower = (upperRamReadResult, lowerData)
    where

    upperD = regEn 0 en upper

    --Swap
    upperData = mux (not <$> regEn False en stage) upperD             lowerRamReadResult
    lowerData = mux (not <$> regEn False en stage) lowerRamReadResult upperD

    --Buffer
    lowerRamReadResult = blockRamPow2 (repeat 0 :: Vec (2 ^ n) a) address 
        $ mux en (Just <$> bundle (address, lower)) (pure Nothing)
    upperRamReadResult = blockRamPow2 (repeat 0 :: Vec (2 ^ n) a) (regEn 0 en address)
        $ mux en (Just <$> bundle (regEn 0 en address, upperData)) (pure Nothing)

--Decimation in time
--2^(n + 1) == size of FFT / 2 == number of butterfly input pairs
-- | A step in the serial FFT decimation in time algorithm. Consumes and produces two complex samples per cycle. 
fftSerialDITStep
    :: forall dom n a. (HiddenClockResetEnable dom, KnownNat n, Num a, NFDataX a)
    => Vec (2 ^ (n + 1)) (Complex a)     -- ^ Precomputed twiddle factors
    -> Signal dom Bool                   -- ^ Input enable signal
    -> Signal dom (Complex a, Complex a) -- ^ Pair of input samples
    -> Signal dom (Complex a, Complex a) -- ^ Pair of output samples
fftSerialDITStep twiddles en input = bundle (butterflyHighOutput, butterflyLowOutput)
    where

    --The state
    counter :: Signal dom (BitVector (n + 1))
    counter =  count 0 en

    (stage :: Signal dom Bool, address :: Signal dom (Unsigned n)) 
        = unbundle $ bitCoerce <$> counter

    --The FIFOs
    (upperRamReadResult, lowerData) = fftReorder en stage address (fst <$> input) (snd <$> input)

    --The butterfly
    twiddlesRot = rotateLeftS twiddles (SNat @ (2 ^ n))
    twiddle     = (twiddlesRot !!) <$> (regEn 0 en $ regEn 0 en counter)

    (butterflyHighOutput, butterflyLowOutput) 
        = ditButterfly twiddle upperRamReadResult (regEn 0 en lowerData)

--Decimation in frequency
--2^(n + 1) == size of FFT / 2 == number of butterfly input pairs
-- | A step in the serial FFT decimation in frequency algorithm. Consumes and produces two complex samples per cycle. 
fftSerialDIFStep
    :: forall dom n a. (HiddenClockResetEnable dom, KnownNat n, Num a, NFDataX a)
    => Vec (2 ^ (n + 1)) (Complex a) -- ^ Precomputed twiddle factors
    -> Signal dom Bool                   -- ^ Input enable signal
    -> Signal dom (Complex a, Complex a) -- ^ Pair of input samples
    -> Signal dom (Complex a, Complex a) -- ^ Pair of output samples
fftSerialDIFStep twiddles en input = bundle (upperRamReadResult, regEn 0 en lowerData)
    where

    --The state
    counter :: Signal dom (BitVector (n + 1))
    counter =  count 0 en

    (stage :: Signal dom Bool, address :: Signal dom (Unsigned n)) 
        = unbundle $ bitCoerce <$> counter

    --The butterfly
    twiddle = (twiddles !!) <$> counter

    (butterflyHighOutput, butterflyLowOutput) 
        = difButterfly twiddle (fmap fst input) (fmap snd input)

    --The FIFOs
    (upperRamReadResult, lowerData) = fftReorder en stage address butterflyHighOutput butterflyLowOutput

