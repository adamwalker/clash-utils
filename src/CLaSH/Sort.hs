{-# LANGUAGE ScopedTypeVariables #-}
module CLaSH.Sort where

import CLaSH.Prelude

compareAndSwap :: (Ord a) => a -> a -> (a, a)
compareAndSwap x y 
    | x > y     = (x, y)
    | otherwise = (y, x)

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

bitonicSorterExample :: forall a. (Ord a) => Vec 16 a -> Vec 16 a
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

