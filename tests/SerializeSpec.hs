module SerializeSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable)
import Test.Hspec
import Test.QuickCheck

import Data.Serialize (runGet, runPut)
import Clash.Serialize

--BitVector serialization
spec = describe "Serialize/deserialize" $ 
    specify "deserialize . serialize = id" $ property prop_serialize

prop_serialize :: BitVector 256 -> Bool
prop_serialize bv = Right bv == result
    where
    result = runGet getBV $ runPut $ putBV bv
