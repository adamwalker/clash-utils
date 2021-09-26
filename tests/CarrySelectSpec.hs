module CarrySelectSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable)
import Test.Hspec
import Test.QuickCheck

import Clash.Arithmetic.CarrySelect

spec = describe "Carry select" $ do
    specify "Adds" $ property prop_carrySelectAdder

prop_carrySelectAdder :: BitVector 512 -> BitVector 512 -> Property
prop_carrySelectAdder x y = carrySelectAdderExample x y === x + y
