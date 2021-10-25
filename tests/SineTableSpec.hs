module SineTableSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, UFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable, sample, System, fromList)
import Test.Hspec
import Test.QuickCheck hiding (sample)

import Clash.DSP.SineTable

approxEqual :: Double -> Double -> Bool
approxEqual x y = abs (x - y) < 0.001

spec = describe "Sine table" $ do
    specify "Outputs the correct values" $ property prop_SineTable

sines' :: Vec 32 (UFixed 0 15)
sines' = $(listToVecTH $ sines 32)

prop_SineTable :: Bool
prop_SineTable = and $ zipWith approxEqual expect $ map realToFrac result
    where
    expect :: [Double]
    expect 
        = take 128
        $ map (\x -> sin(2*pi*(2*(fromIntegral x)+1)/(8*fromIntegral 32)))
        $ [0..31]
    result = take 128 $ drop 1 $ sample @System $ sineTable sines' $ fromList $ [0..127] ++ repeat 0

