module PrefixSumSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockReset)
import Test.Hspec
import Test.QuickCheck

import Clash.PrefixSum

--Misc
spec = describe "Prefix sums" $ do
    specify "Prefix sum works for addition" $ property prop_prefixSumParallel

prop_prefixSumParallel :: (Vec 32 Int) -> Bool
prop_prefixSumParallel vec@(x :> rest) = Clash.scanl (+) x rest == prefixSumParallel32 (+) vec

