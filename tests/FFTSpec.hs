module FFTSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockReset)
import Test.Hspec
import Test.QuickCheck

import qualified Numeric.FFT as FFT
import qualified Data.Complex as C
import Clash.DSP.Complex (Complex, fromComplex, toComplex)
import Clash.DSP.FFT

--FFT
spec = describe "Parallel FFTs" $ do
    specify "Recursive decimation in time equals known good implementation"      $ property prop_fftDITRec
    specify "Recursive decimation in frequency equals known good implementation" $ property prop_fftDIFRec
    specify "Iterative decimation in time equals known good implementation"      $ property prop_fftDITIter
    specify "Iterative decimtaion in frequency equals known good implementation" $ property prop_fftDIFIter

approxEqual :: Double -> Double -> Bool
approxEqual x y = abs (x - y) < 0.0001

approxEqualComplex (a C.:+ b) (c C.:+ d) = approxEqual a c && approxEqual b d

twiddles :: Vec 8 (Complex Double)
twiddles = $(listToVecTH (twiddleFactors 8))

prop_fftDITRec :: Vec 16 (C.Complex Double) -> Bool
prop_fftDITRec vec = and $ zipWith approxEqualComplex (map toComplex (Clash.toList (fftDITRec twiddles (Clash.map fromComplex vec)))) (FFT.fft (Clash.toList vec))

prop_fftDIFRec :: Vec 16 (C.Complex Double) -> Bool
prop_fftDIFRec vec = and $ zipWith approxEqualComplex (map toComplex (Clash.toList (fftDIFRec twiddles (Clash.map fromComplex vec)))) (FFT.fft (Clash.toList vec))

prop_fftDITIter :: Vec 16 (C.Complex Double) -> Bool
prop_fftDITIter vec = and $ zipWith approxEqualComplex (map toComplex (Clash.toList (fftDITIter twiddles (Clash.map fromComplex vec)))) (FFT.fft (Clash.toList vec))

prop_fftDIFIter :: Vec 16 (C.Complex Double) -> Bool
prop_fftDIFIter vec = and $ zipWith approxEqualComplex (map toComplex (Clash.toList (fftDIFIter twiddles (Clash.map fromComplex vec)))) (FFT.fft (Clash.toList vec))
