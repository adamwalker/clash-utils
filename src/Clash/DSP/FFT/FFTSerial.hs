{-| Radix 2 complex-to-complex Cooley-Tukey FFTs. https://en.wikipedia.org/wiki/Cooley%E2%80%93Tukey_FFT_algorithm.
    The FFTs in this module are serial, saving multiplers and routing resources. They operate on and produce two complex numbers at a time. 
-}
module Clash.DSP.FFT.FFTSerial (
    fftSerialDITStep,
    fftSerialDIT,
    fftSerialDIFStep,
    fftSerialDIF
    ) where

import Clash.Prelude
import Data.Function

import Clash.Counter(count)
import Clash.DSP.Complex
import Clash.DSP.FFT.Twiddle(halveTwiddles)

fftBase 
    :: (HiddenClockResetEnable dom, Num a, NFDataX a) 
    => Signal dom Bool 
    -> Signal dom (Complex a, Complex a) 
    -> Signal dom (Complex a, Complex a)
fftBase en = regEn (0, 0) en . fmap func
    where
    func (x, y) = (x + y, x - y)

fftCounter
    :: forall n dom. (HiddenClockResetEnable dom, KnownNat n)
    => Signal dom Bool
    -> ((Signal dom Bool, Signal dom (Unsigned n)), Signal dom (BitVector (n + 1)))
fftCounter en = ((stage, address), counter)
    where

    counter :: Signal dom (BitVector (n + 1))
    counter = count 0 en

    (stage' :: Signal dom (BitVector 1), address' :: Signal dom (BitVector n)) = unbundle $ split <$> counter

    stage :: Signal dom Bool
    stage = unpack <$> stage'

    address :: Signal dom (Unsigned n)
    address = unpack <$> address'

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
    -> (Signal dom Bool, Signal dom (Unsigned n))
    -> Signal dom a
    -> Signal dom a
    -> (Signal dom a, Signal dom a)
fftReorder en (stage, address) upper lower = (upperRamReadResult, lowerData)
    where

    --Swap
    upperData = mux (not <$> regEn False en stage) (regEn 0 en upper) lowerRamReadResult
    lowerData = mux (not <$> regEn False en stage) lowerRamReadResult (regEn 0 en upper)

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
    (address, counter) = fftCounter @n en

    --The FIFOs
    (upperRamReadResult, lowerData) = fftReorder en address (fst <$> input) (snd <$> input)

    --The butterfly
    butterflyHighInput = upperRamReadResult
    butterflyLowInput  = regEn 0 en lowerData

    twiddlesRot = rotateLeftS twiddles (SNat @ (2 ^ n))
    twiddle     = (twiddlesRot !!) <$> (regEn 0 en $ regEn 0 en counter)
    twiddled    = butterflyLowInput * twiddle

    butterflyHighOutput = butterflyHighInput + twiddled
    butterflyLowOutput  = butterflyHighInput - twiddled 

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
    (address, counter) = fftCounter @n en

    --The butterfly
    butterflyHighOutput          = fmap fst input + fmap snd input
    butterflyLowOutputPreTwiddle = fmap fst input - fmap snd input

    twiddle            = (twiddles !!) <$> counter
    butterflyLowOutput = butterflyLowOutputPreTwiddle * twiddle

    --The FIFOs
    (upperRamReadResult, lowerData) = fftReorder en address butterflyHighOutput butterflyLowOutput

-- | Example serial FFT decimation in time algorithm. Consumes and produces two complex samples per cycle. Note that both the input and output samples must be supplied in a weird order. See the tests.
fftSerialDIT
    :: forall dom a. (HiddenClockResetEnable dom, Num a, NFDataX a)
    => Vec 4 (Complex a)                 -- ^ Precomputed twiddle factors
    -> Signal dom Bool                   -- ^ Input enable signal
    -> Signal dom (Complex a, Complex a) -- ^ Pair of input samples
    -> Signal dom (Complex a, Complex a) -- ^ Pair of output samples
fftSerialDIT twiddles en input  
    = fftBase en input
    & fftSerialDITStep cexp2    stage2En
    & fftSerialDITStep twiddles stage3En

    where

    stage2En = register False en
    stage3En = last $ generate (SNat @ 3) (register False) stage2En

    cexp2 :: Vec 2 (Complex a)
    cexp2 = halveTwiddles twiddles

-- | Example serial FFT decimation in frequency algorithm. Consumes and produces two complex samples per cycle. Note that both the input and output samples must be supplied in a weird order. See the tests.
fftSerialDIF
    :: forall dom a. (HiddenClockResetEnable dom, Num a, NFDataX a)
    => Vec 4 (Complex a)                 -- ^ Precomputed twiddle factors
    -> Signal dom Bool                   -- ^ Input enable signal
    -> Signal dom (Complex a, Complex a) -- ^ Pair of input samples
    -> Signal dom (Complex a, Complex a) -- ^ Pair of output samples
fftSerialDIF twiddles en input 
    = fftSerialDIFStep twiddles en input
    & fftSerialDIFStep cexp2    stage2En 
    & fftBase                   stage3En 

    where

    stage2En = last $ generate (SNat @ 4) (register False) en
    stage3En = last $ generate (SNat @ 3) (register False) stage2En

    cexp2 :: Vec 2 (Complex a)
    cexp2 = halveTwiddles twiddles

