module MultiplierSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockReset, extend)
import Test.Hspec
import Test.QuickCheck

import Clash.Arithmetic.Multiplier

spec = describe "Multiplier lookahead" $ do
    specify "unsigned" $ property prop_unsigned
    specify "signed"   $ property prop_signed

prop_unsigned :: BitVector 8 -> BitVector 8 -> Bool
prop_unsigned x y = multiply x y == (extend x * extend y)

prop_signed :: BitVector 8 -> BitVector 8 -> Bool
prop_signed x y = multiplySigned x y == pack ((extend (unpack x) :: Signed 16) * extend (unpack y))
