module PrefixSumSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable)
import Test.Hspec
import Test.QuickCheck

import Clash.PrefixSum

--Misc
spec = describe "Prefix sums" $ do
    specify "Parallel prefix sum works for addition"       $ property prop_prefixSumParallel
    specify "Work efficient prefix sum works for addition" $ property prop_prefixSumWorkEfficient

prop_prefixSumParallel :: (Vec 32 Int) -> Property
prop_prefixSumParallel vec@(x :> rest) = Clash.scanl (+) x rest === prefixSumParallel32 (+) vec

prop_prefixSumWorkEfficient :: (Vec 32 Int) -> Property
prop_prefixSumWorkEfficient vec@(x :> rest) = Clash.scanl (+) x rest === prefixSumWorkEfficient32 (+) vec
