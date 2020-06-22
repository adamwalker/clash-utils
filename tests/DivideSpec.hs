module DivideSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable)
import Test.Hspec
import Test.QuickCheck

import Clash.Arithmetic.Divide

--Divider
spec = describe "Divider" $ 
    specify "divides" $ property prop_Divider
    
prop_Divider :: BitVector 32 -> BitVector 32 -> Property
prop_Divider x y = (y /= 0) ==> (q == q' && r == r')
    where
    (q, r)   = quotRem x y
    (q', r') = combDivide x y

