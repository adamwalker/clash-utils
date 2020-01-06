module InsertionSortSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable, toList, postscanl)
import Test.Hspec
import Test.QuickCheck
import Data.List (sort, delete)
import Clash.Sort.Insertion

spec = describe "Insertion sort" $ do
    specify "sorts"   $ property prop_insertionSort
    specify "deletes" $ property prop_sortedDelete

prop_insertionSort :: Integer -> Vec 16 (NonNegative Integer) -> Property
prop_insertionSort x vec = expect === result
    where
    result    = toList $ sortedInsert x sortedVec
    expect    = sort $ x : toList sortedVec
    sortedVec = postscanl (+) 0 $ Clash.map getNonNegative vec

prop_sortedDelete :: Property
prop_sortedDelete 
    = forAll arbitrary $ \(vec :: Vec 10 (Positive Integer)) -> 
        let sortedVec = postscanl (+) 0 $ Clash.map getPositive vec
        in forAll (elements (toList sortedVec)) $ \x -> 
            let
                result    = toList $ sortedDelete x sortedVec
                expect    = take 9 $ Data.List.delete x (toList sortedVec)
            in  expect === result

