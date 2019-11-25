module StreamTestSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable, NFDataX, System, fromList, (.&&.), sample)
import Test.Hspec
import Test.QuickCheck hiding ((.&&.), sample)

import Clash.Stream.Test

spec = describe "Stream test utils" $ do
    specify "test the tests" $ property $ prop_test

prop_test = forAll (listOf (listOf1 arbitrary)) $ \(lists :: [[Int]]) -> 
    fromStreamList (toStreamList lists) == lists
