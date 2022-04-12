module Clash.DSP.FFT.Example (
        fftSerialDIT,
        fftSerialDIF
    ) where

import Clash.Prelude
import Data.Function
import Clash.DSP.Complex
import Clash.DSP.FFT.Serial
import Clash.DSP.FFT.Twiddle(halveTwiddles)
import Clash.DSP.FFT.Butterfly

-- | Example serial FFT decimation in time algorithm. Consumes and produces two complex samples per cycle. Note that both the input and output samples must be supplied in a weird order. See the tests.
fftSerialDIT
    :: forall dom a. (HiddenClockResetEnable dom, Num a, NFDataX a, Default a)
    => Vec 4 (Complex a)                 -- ^ Precomputed twiddle factors
    -> Signal dom Bool                   -- ^ Input enable signal
    -> Signal dom (Complex a, Complex a) -- ^ Pair of input samples
    -> Signal dom (Complex a, Complex a) -- ^ Pair of output samples
fftSerialDIT twiddles en input  
    = fftBase en input
    & fftSerialDITStep ditButterfly cexp2    stage2En
    & fftSerialDITStep ditButterfly twiddles stage3En

    where

    stage2En = register False en
    stage3En = last $ generate (SNat @3) (register False) stage2En

    cexp2 :: Vec 2 (Complex a)
    cexp2 = halveTwiddles twiddles

-- | Example serial FFT decimation in frequency algorithm. Consumes and produces two complex samples per cycle. Note that both the input and output samples must be supplied in a weird order. See the tests.
fftSerialDIF
    :: forall dom a. (HiddenClockResetEnable dom, Num a, NFDataX a, Default a)
    => Vec 4 (Complex a)                 -- ^ Precomputed twiddle factors
    -> Signal dom Bool                   -- ^ Input enable signal
    -> Signal dom (Complex a, Complex a) -- ^ Pair of input samples
    -> Signal dom (Complex a, Complex a) -- ^ Pair of output samples
fftSerialDIF twiddles en input 
    = fftSerialDIFStep difButterfly twiddles en input
    & fftSerialDIFStep difButterfly cexp2    stage2En 
    & fftBase                                stage3En 

    where

    stage2En = last $ generate (SNat @4) (register False) en
    stage3En = last $ generate (SNat @3) (register False) stage2En

    cexp2 :: Vec 2 (Complex a)
    cexp2 = halveTwiddles twiddles

