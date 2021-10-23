module ComplexSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable, type (+), extend, fromList, sample, System)
import Test.Hspec
import Test.QuickCheck hiding (sample)

import Clash.DSP.Complex

spec = describe "Complex numbers" $ do
    specify "Three multipler complex multiplication gives the correct result" $ property prop_complex3Mul
    specify "Pipelined multiplier gives the correct result"                   $ property prop_complexPipe
    specify "Pipelined three multiplier multiplier gives the correct result"  $ property prop_complexPipe3Mul

prop_complex3Mul :: Complex (Signed 16) -> Complex (Signed 16) -> Property
prop_complex3Mul x y = expect === (Clash.truncateB <$> result)
    where
    expect :: Complex (Signed 33)
    expect =  cMul x y 
    result :: Complex (Signed 34)
    result =  cMul3 x y

prop_complexPipe :: [(Complex (Signed 16), Complex (Signed 16))] -> Property
prop_complexPipe cs = noShrinking $ expect === take (length cs) (drop 3 result)
    where
    expect :: [Complex (Signed 33)]
    expect =  map (uncurry cMul) cs
    result :: [Complex (Signed 33)]
    result =  sample @System $ cMulPipe (pure True) (fromList $ 0 : map fst cs ++ repeat 0) (fromList $ 0 : map snd cs ++ repeat 0)

prop_complexPipe3Mul :: [(Complex (Signed 16), Complex (Signed 16))] -> Property
prop_complexPipe3Mul cs = noShrinking $ expect === map (fmap Clash.truncateB) (take (length cs) (drop 4 result))
    where
    expect :: [Complex (Signed 33)]
    expect =  map (uncurry cMul) cs
    result :: [Complex (Signed 34)]
    result =  sample @System $ cMul3Pipe (pure True) (fromList $ 0 : map fst cs ++ repeat 0) (fromList $ 0 : map snd cs ++ repeat 0)

