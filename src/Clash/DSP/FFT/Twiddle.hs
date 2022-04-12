module Clash.DSP.FFT.Twiddle (
    twiddleFactors,
    halveTwiddles
) where

import Clash.Prelude
import qualified Data.Complex as C
import qualified Prelude as P
import Clash.DSP.Complex

-- | Calculate FFT `twiddle` factors. You probably want to do this with template Haskell to ensure they are calculated at compile time.
twiddleFactors 
    :: Int              -- ^ Twiddle factor vector length
    -> [Complex Double] -- ^ Twiddle factors
twiddleFactors num = P.take num $ [fromComplex $ C.cis $ (-1) * P.pi * fromIntegral i / (fromIntegral num) | i <- [0..]]

-- | Take every second element of a vector of twiddle factors.
halveTwiddles 
    :: KnownNat n 
    => Vec (2 * n) a -- ^ Twiddle factors
    -> Vec n a       -- ^ Halved output twiddle factors
halveTwiddles vec = transpose (unconcat (SNat @2) vec) !! 0

