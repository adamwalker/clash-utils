module Clash.Sort.Insertion (
        sortedInsert,
        sortedDelete
    ) where

import Clash.Prelude

import Data.Bool

-- | Efficiently insert an item into a sorted vector, assuming it is not already present
sortedInsert 
    :: forall n a
    .  Ord a 
    => a             -- ^ Item to insert
    -> Vec (n + 1) a -- ^ Sorted input vector
    -> Vec (n + 2) a -- ^ Sorted output vector containing new item
sortedInsert x xs = firstItem :> (middleItems :< lastItem)
    where
    comparisons :: Vec (n + 1) Bool
    comparisons =  map (< x) xs

    cPairs :: Vec n (Bool, Bool)
    cPairs =  zip (tail comparisons) (init comparisons)

    vPairs :: Vec n (a, a)
    vPairs =  zip (tail xs) (init xs)

    middleItems :: Vec n a
    middleItems =  zipWith func cPairs vPairs
        where
        func :: (Bool, Bool) -> (a, a) -> a
        func (True,  True)  (y, _) = y
        func (False, True)  (_, _) = x
        func (True,  False) _      = errorX "sortedInsert: impossible!"
        func (False, False) (_, y) = y

    firstItem :: a
    firstItem =  bool x (head xs) (head comparisons)

    lastItem :: a
    lastItem =  bool (last xs) x (last comparisons)

-- | Efficiently delete an item from a sorted vector, assuming the item is in the vector
sortedDelete 
    :: forall n a
    .  Ord a 
    => a             -- ^ Item to delete
    -> Vec (n + 1) a -- ^ Sorted input vector
    -> Vec n a       -- ^ Vector with item deleted, or the last element deleted if the item wasn't present
sortedDelete x xs = res
    where

    comparisons :: Vec (n + 1) Bool
    comparisons =  map (< x) xs

    vPairs :: Vec n (a, a)
    vPairs =  zip (tail xs) (init xs)

    res :: Vec n a
    res = zipWith func (init comparisons) vPairs
        where
        func True  (_, y) = y
        func False (y, _) = y

