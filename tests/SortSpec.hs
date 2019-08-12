module SortSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable)
import Test.Hspec
import Test.QuickCheck
import Data.List
import Clash.Sort

--Bitonic sorting network
spec = describe "Bitonic sort" $ do
    specify "sorts"          $ property prop_BitonicSort
    specify "sorts any size" $ property prop_BitonicSortGeneric

prop_BitonicSort :: Vec 16 (Signed 32) -> Bool
prop_BitonicSort vec = Clash.toList (bitonicSorterExample vec) == reverse (sort (Clash.toList vec))

--Length generic bitonic sorter
prop_BitonicSortGeneric :: Vec 64 (Signed 32) -> Bool
prop_BitonicSortGeneric vec = Clash.toList (bitonicSorter vec) == reverse (sort (Clash.toList vec))

