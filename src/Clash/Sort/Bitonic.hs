{-# LANGUAGE UndecidableInstances #-}

{-| 
    Bitonic sorting network. See <https://en.wikipedia.org/wiki/Bitonic_sorter> and <http://www.iti.fh-flensburg.de/lang/algorithmen/sortieren/bitonic/bitonicen.htm>. 

    __FPGA proven__ but only for small vectors since it is (currently) purely combinational.
-}
module Clash.Sort.Bitonic (
    bitonicMerge,
    bitonicSort, 
    bitonicSorterExample,
    bitonicSorter
    ) where

import Clash.Prelude

import Data.Singletons.Prelude (Apply, TyFun, type (@@))
import Data.Proxy
import Data.Kind (Type)

compareAndSwap :: (Ord a) => a -> a -> (a, a)
compareAndSwap x y 
    | x > y     = (x, y)
    | otherwise = (y, x)

{-| Bitonic merge. Parameterised by the recursive step - to do a bitonic merge on a vector half the size - because Clash can not handle recursive functions. -}
bitonicMerge 
    :: forall n a. (Ord a , KnownNat n)
    => (Vec n a -> Vec n a) -- ^ The recursive step
    -> Vec (2 * n) a        -- ^ Input vector 
    -> Vec (2 * n) a        -- ^ Output vector
bitonicMerge recurse input = recurse firstBitonic ++ recurse secondBitonic
    where
    partitioned :: Vec 2 (Vec n a)
    partitioned = unconcatI input
    firstBitonic, secondBitonic :: Vec n a
    (firstBitonic, secondBitonic) = unzip $ zipWith compareAndSwap (partitioned !! 0) (partitioned !! 1)

{-| Bitonic sort. Parameterised by both the bitonic merge and the recursive step - a bitonic sort of half the size. -}
bitonicSort 
    :: forall n a. (KnownNat n, Ord a)
    => (Vec n a -> Vec n a)             -- ^ The recursive step
    -> (Vec (2 * n) a -> Vec (2 * n) a) -- ^ Merge step
    -> Vec (2 * n) a                    -- ^ Input vector
    -> Vec (2 * n) a                    -- ^ Output vector
bitonicSort recurse merge input = merge $ firstSorted ++ secondSorted
    where
    split :: Vec 2 (Vec n a)
    split =  unconcatI input

    firstSorted, secondSorted :: Vec n a
    firstSorted  = recurse $ split !! 0
    secondSorted = reverse $ recurse $ split !! 1

{-| An example 16 element bitonic sorter -}
bitonicSorterExample 
    :: forall a. (Ord a) 
    => Vec 16 a -- ^ Input vector
    -> Vec 16 a -- ^ Sorted output vector
bitonicSorterExample = sort16
    where
    sort16 = bitonicSort sort8 merge16
    merge16 = bitonicMerge merge8

    sort8  = bitonicSort  sort4  merge8
    merge8 = bitonicMerge merge4

    sort4  = bitonicSort  sort2 merge4
    merge4 = bitonicMerge merge2

    sort2  = bitonicSort  id merge2
    merge2 = bitonicMerge id 
    

{-| Arbitrary length bitonic sorter -}
type ExpVec k a = Vec (2 ^ k) a

data SplitHalf (a :: Type) (f :: TyFun Nat Type) :: Type
type instance Apply (SplitHalf a) k = (ExpVec k a -> ExpVec k a, ExpVec (k + 1) a -> ExpVec (k + 1) a)

{-| Length generic bitonic sorter -}
bitonicSorter 
    :: forall k a . (Ord a, KnownNat k) 
    => Vec (2 ^ k) a -- ^ Input vector
    -> Vec (2 ^ k) a -- ^ Sorted output vector
bitonicSorter = fst $ dfold (Proxy @ (SplitHalf a)) step base (replicate (SNat @ k) ())
    where
    step :: SNat l -> () -> SplitHalf a @@ l -> SplitHalf a @@ (l + 1)
    step SNat _ (sort, merge) = (bitonicSort sort merge, bitonicMerge merge)

    base = (id, bitonicMerge id)

