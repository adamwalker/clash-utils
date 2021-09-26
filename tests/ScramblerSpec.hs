module ScramblerSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable, System)
import Test.Hspec
import Test.QuickCheck

import Clash.Scrambler

--Scrambler
spec = describe "Scrambler" $ do
    specify "serial descrambler . serial scrambler == id" $ property prop_scramblerSerial
    specify "parallel descrambler . parallel scrambler == id" $ property prop_scramblerParallel

prop_scramblerSerial :: BitVector 20 -> BitVector 19 -> [Bool] -> Bool
prop_scramblerSerial initial (poly :: BitVector 19) input 
    = take (length input) (simulate @System combined input) == input
    where
    combined :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Bool
    combined s = serialScrambler initial poly $ serialDescrambler initial poly s

prop_scramblerParallel :: BitVector 20 -> BitVector 19 -> [Vec 16 Bool] -> Bool
prop_scramblerParallel initial (poly :: BitVector 19) input 
    = take (length input) (simulate @System combined input) == input
    where
    combined :: HiddenClockResetEnable dom => Signal dom (Vec 16 Bool) -> Signal dom (Vec 16 Bool)
    combined s = parallelScrambler initial poly $ parallelDescrambler initial poly s
