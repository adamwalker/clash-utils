module FFTSerialSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable, System)
import Test.Hspec
import Test.QuickCheck

import qualified Numeric.FFT as FFT
import qualified Data.Complex as C
import Clash.DSP.Complex (Complex, fromComplex, toComplex)
import Clash.DSP.FFTSerial
import Clash.DSP.FFT

--serial FFT
spec = describe "Serial FFTs" $ do
    specify "Decimation in time equals known good implementation"      $ property prop_fftSerialDIT
    specify "Decimation in frequency equals known good implementation" $ property prop_fftSerialDIF

approxEqual :: Double -> Double -> Bool
approxEqual x y = abs (x - y) < 0.0001

approxEqualComplex (a C.:+ b) (c C.:+ d) = approxEqual a c && approxEqual b d

twiddles4 :: Vec 4 (Complex Double)
twiddles4 = $(listToVecTH (twiddleFactors 4))

ditInputReorder :: Vec 8 a -> Vec 4 (a, a)
ditInputReorder (a :> b :> c :> d :> e :> f :> g :> h :> Nil) = (a, e) :> (c, g) :> (b, f) :> (d, h) :> Nil

ditOutputReorder :: [(a, a)] -> [a]
ditOutputReorder ((a, b) : (c, d) : (e, f) : (g, h) : _) = a : c : e : g : b : d : f : h : []

difInputReorder :: Vec 8 a -> Vec 4 (a, a)
difInputReorder (a :> b :> c :> d :> e :> f :> g :> h :> Nil) = (a, e) :> (b, f) :> (c, g) :> (d, h) :> Nil

difOutputReorder :: [(a, a)] -> [a]
difOutputReorder ((a, b) : (c, d) : (e, f) : (g, h) : _) = a : e : c : g : b : f : d : h : []

prop_fftSerialDIT :: Vec 8 (C.Complex Double) -> Bool
prop_fftSerialDIT vec = and $ zipWith approxEqualComplex (map toComplex result) (FFT.fft (Clash.toList vec))
    where
    result = ditOutputReorder $ drop 8 $ simulate_lazy @System (fftSerialDIT twiddles4 (pure True)) $ (Clash.toList (ditInputReorder (Clash.map fromComplex vec))) ++ repeat (0, 0)

prop_fftSerialDIF :: Vec 8 (C.Complex Double) -> Bool
prop_fftSerialDIF vec = and $ zipWith approxEqualComplex (map toComplex result) (FFT.fft (Clash.toList vec))
    where
    result = difOutputReorder $ drop 8 $ simulate_lazy @System (fftSerialDIF twiddles4 (pure True)) $ (Clash.toList (difInputReorder (Clash.map fromComplex vec))) ++ repeat (0, 0)

