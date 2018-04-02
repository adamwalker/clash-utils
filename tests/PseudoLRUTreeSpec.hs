module PseudoLRUTreeSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockReset)
import Test.Hspec
import Test.QuickCheck

import Data.List
import Clash.PseudoLRUTree

--Pseudu lru tree pseudo-tests
spec = describe "Pseudo LRU tree" $ do
    specify "cycles through all possibilities" $ property prop_plru
    specify "both implementations same"        $ property prop_plruSame
    specify "update same index idempotent"     $ property prop_plruIdempotent
    specify "simple case with single bit"      $ property prop_plruSimpleCase

prop_plru :: Vec 15 Bool -> Bool
prop_plru tree = reordered == [0..15]
    where
    trees     = Clash.iterate (SNat @ 16) func tree
        where
        func tree = updateWay (getOldestWay tree) tree
    reordered = sort $ Clash.toList $ Clash.map (fromIntegral . pack) $ Clash.map getOldestWay trees

prop_plruSame :: Vec 15 Bool -> Bool
prop_plruSame tree = updateOldestWay tree == (oldest, newTree)
    where
    oldest  = getOldestWay tree
    newTree = updateWay oldest tree

prop_plruIdempotent :: Vec 15 Bool -> Vec 4 Bool -> Bool
prop_plruIdempotent tree idx = updateWay idx tree == updateWay idx (updateWay idx tree)

prop_plruSimpleCase :: Vec 1 Bool -> Bool
prop_plruSimpleCase tree = updateWay (getOldestWay tree) tree == Clash.map not tree

