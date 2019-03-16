module CarryLookaheadSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockReset)
import Test.Hspec
import Test.QuickCheck

import Clash.CarryLookahead

--Misc
spec = describe "Carry lookahead" $ do
    specify "Kogge-Stone adder" $ property prop_koggeStoneAdder

prop_koggeStoneAdder :: Bool -> BitVector 32 -> BitVector 32 -> Bool
prop_koggeStoneAdder cIn x y = snd (koggeStone cIn x y) == x + y + (if cIn then 1 else 0)

