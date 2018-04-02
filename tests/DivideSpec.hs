module DivideSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockReset)
import Test.Hspec
import Test.QuickCheck

import Clash.Divide

--Divider
spec = describe "Divder" $ 
    specify "divides" $ property prop_Divider
    
prop_Divider :: BitVector 32 -> BitVector 32 -> Bool
prop_Divider x y = q == q' && r == r'
    where
    (q, r)   = quotRem x y
    (q', r') = combDivide x y

