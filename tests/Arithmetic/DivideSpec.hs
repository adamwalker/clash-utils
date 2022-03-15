module Arithmetic.DivideSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable)
import Test.Hspec
import Test.QuickCheck

import Clash.Arithmetic.Divide

--Divider
spec = describe "Divider" $ do
    specify "restoring division"     $ property $ prop_divider combDivide
    specify "non-restoring division" $ property $ prop_divider combNRDivide
    
prop_divider 
    :: (BitVector 32 -> BitVector 32 -> (BitVector 32, BitVector 32))
    -> BitVector 32 
    -> BitVector 32 
    -> Property
prop_divider hwDiv x y = (y /= 0) ==> (q == q' && r == r')
    where
    (q, r)   = quotRem x y
    (q', r') = hwDiv x y

