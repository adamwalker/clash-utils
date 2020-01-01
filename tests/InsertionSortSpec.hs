module InsertionSortSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable, toList, postscanl, map)
import Test.Hspec
import Test.QuickCheck
import Data.List (sort)
import Clash.Sort.Insertion

spec = describe "Insertion sort" $ do
    specify "sorts" $ property prop_insertionSort

prop_insertionSort :: Prelude.Integer -> Vec 16 (NonNegative Prelude.Integer) -> Property
prop_insertionSort x vec = expect === result
    where
    result    = toList $ sortedInsert x sortedVec
    expect    = sort $ x : toList sortedVec
    sortedVec = postscanl (+) 0 $ Clash.map getNonNegative vec
