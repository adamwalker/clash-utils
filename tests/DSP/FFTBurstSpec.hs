module DSP.FFTBurstSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable, System, sampleN)
import Test.Hspec
import Test.QuickCheck

import qualified Numeric.FFT as FFT
import qualified Data.Complex as C
import Clash.DSP.Complex (Complex, fromComplex, toComplex)
import Clash.DSP.FFT.Burst
import Clash.DSP.FFT.Twiddle

--FFT
spec = describe "Burst FFTs" $ do
    specify "Burst FFT equals known good implementation" $ property prop_fftBurst

twiddles4 :: Vec 4 (Complex Double)
twiddles4 = $(listToVecTH (twiddleFactors 4))

inputReorder :: Vec 8 a -> (Vec 4 a, Vec 4 a)
inputReorder (a :> b :> c :> d :> e :> f :> g :> h :> Nil)
    = (a :> d :> f :> g :> Nil, b :> c :> e :> h :>  Nil)

difOutputReorder :: [(a, a)] -> [a]
difOutputReorder ((a, b) : (c, d) : (e, f) : (g, h) : _) = [a, e, c, g, b, f, d, h]

approxEqual :: Double -> Double -> Bool
approxEqual x y = abs (x - y) < 0.0001

approxEqualComplex (a C.:+ b) (c C.:+ d) = approxEqual a c && approxEqual b d

prop_fftBurst :: Vec 8 (C.Complex Double) -> Bool
prop_fftBurst vec = and $ zipWith approxEqualComplex (map toComplex result) (FFT.fft (Clash.toList vec))
    where
    result 
        = difOutputReorder 
        $ drop 10 
        $ sampleN @System 14
        $ bundle
        $ burstFFT twiddles4
        $ inputReorder (Clash.map fromComplex vec)

