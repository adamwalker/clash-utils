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
    
    
    

{-| generalised bitonic sorter. -}
type ExpVec k a = Vec (2 ^ k) a

data SplitHalf (a :: *) (f :: TyFun Nat *) :: *
type instance Apply (SplitHalf a) k = (ExpVec k a -> ExpVec k a, ExpVec (k + 1) a -> ExpVec (k + 1) a)

generateBitonicSortN2 :: forall k a . (Ord a, KnownNat k) => SNat k -> ExpVec k a -> ExpVec k a
generateBitonicSortN2 k = fst $ dfold (Proxy :: Proxy (SplitHalf a)) step base (replicate k ())
  where
    step :: SNat l -> () -> SplitHalf a @@ l -> SplitHalf a @@ (l+1)
    step SNat _ (sort, merge) = (bitonicSort sort merge, bitonicMerge merge)

    base = (id, bitonicMerge id)

generateBitonicSortN2Base :: (KnownNat k, Ord a) => ExpVec k a -> ExpVec k a
generateBitonicSortN2Base = generateBitonicSortN2 (snatProxy Proxy)



{-| Examples -}
testVec16 :: Num a => Vec 16 a
testVec16 =  9 :> 2 :> 8 :> 6 :> 3 :> 7 :> 0 :> 1 :> 4 :> 5 :> 2 :> 8 :> 6 :> 3 :> 7 :> 0 :> Nil
testVec8 :: Num a => Vec 8 a
testVec8 =  9 :> 2 :> 8 :> 6 :> 3 :> 7 :> 0 :> 1 :> Nil
testVec4 :: Num a => Vec 4 a
testVec4 =  9 :> 2 :> 8 :> 6 :> Nil
testVec2 :: Num a => Vec 2 a
testVec2 =  2 :> 9 :> Nil

sorter16 :: (Ord a) => Vec 16 a -> Vec 16 a
sorter16 = generateBitonicSortN2Base 
sorter8 :: (Ord a) => Vec 8 a -> Vec 8 a
sorter8 = generateBitonicSortN2Base 
sorter4 ::forall a. (Ord a) => Vec 4 a -> Vec 4 a
sorter4 = generateBitonicSortN2Base 
sorter2 :: forall a. (Ord a) => Vec 2 a -> Vec 2 a
sorter2 = generateBitonicSortN2Base 

example16 :: Vec 16 Int
example16 = sorter16 testVec16
example8 :: Vec 8 Int
example8 = sorter8 testVec8
example4 :: Vec 4 Int
example4 = sorter4 testVec4

