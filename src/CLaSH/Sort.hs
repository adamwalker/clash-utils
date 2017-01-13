{-# LANGUAGE ScopedTypeVariables #-}

{-| Bitonic sorting network. See <https://en.wikipedia.org/wiki/Bitonic_sorter> and <http://www.iti.fh-flensburg.de/lang/algorithmen/sortieren/bitonic/bitonicen.htm>. -}
module CLaSH.Sort (
    bitonicMerge,
    bitonicSort, 
    bitonicSorterExample
    ) where

import CLaSH.Prelude

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

{-| An example 16 element bitonic sorter. TODO: this can probably be generalised to any size using the dependently typed fold in the prelude. -}
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

