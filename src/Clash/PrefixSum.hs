module Clash.PrefixSum (
    prefixSumParallelStep,
    prefixSumParallel32,
    prefixSumWorkEfficientStepA,
    prefixSumWorkEfficientStepB,
    prefixSumWorkEfficient32
    ) where

import Clash.Prelude

prefixSumParallelStep 
    :: forall take drop n a
    .  n ~ (take + drop)
    => SNat take
    -> SNat drop
    -> (a -> a -> a) 
    -> Vec n a 
    -> Vec n a
prefixSumParallelStep SNat SNat op inputs = untouched ++ zipWith op left right
    where
    
    untouched :: Vec drop a
    untouched =  takeI inputs

    left      :: Vec take a
    left      =  takeI inputs

    right     :: Vec take a
    right     =  dropI inputs

prefixSumParallel32 :: (a -> a -> a) -> Vec 32 a -> Vec 32 a
prefixSumParallel32 op vec 
    = prefixSumParallelStep (SNat @ 16) (SNat @ 16) op
    $ prefixSumParallelStep (SNat @ 24) (SNat @ 8)  op
    $ prefixSumParallelStep (SNat @ 28) (SNat @ 4)  op
    $ prefixSumParallelStep (SNat @ 30) (SNat @ 2)  op
    $ prefixSumParallelStep (SNat @ 31) (SNat @ 1)  op
    $ vec

prefixSumWorkEfficientStepA
    :: forall level n k o a
    .  (KnownNat k, n ~ ((k * 2) * (2 ^ level)), (o + 1) ~ (2 ^ level))
    => SNat level
    -> (a -> a -> a)
    -> Vec n a
    -> Vec n a
prefixSumWorkEfficientStepA SNat op inputs = concat opped
    where

    groups  :: Vec (k * 2) (Vec (2 ^ level) a)
    groups  =  unconcatI inputs

    pairs   :: Vec k (Vec 2 (Vec (2 ^ level) a))
    pairs   =  unconcatI groups

    opped   :: Vec k (Vec (2 ^ (level + 1)) a)
    opped   =  map func pairs
        where
        func :: Vec 2 (Vec (2 ^ level) a) -> Vec (2 ^ (level + 1)) a
        func (left :> right :> Nil) 
            =  left 
            ++ init right 
            ++ singleton (last left `op` last right)

prefixSumWorkEfficientStepB
    :: forall groupSize g n a 
    .  (KnownNat n, groupSize ~ (g + 1))
    => SNat groupSize
    -> (a -> a -> a)
    -> Vec ((2*n + 2) * groupSize) a
    -> Vec ((2*n + 2) * groupSize) a
prefixSumWorkEfficientStepB SNat op inputs = first ++ ress ++ last
    where

    groups :: Vec (2*n + 2) (Vec groupSize a)
    groups =  unconcatI inputs

    first, last :: Vec groupSize a
    mid :: Vec (2 * n) (Vec groupSize a)
    first :> (mid :< last) = groups

    pairs :: Vec n (Vec 2 (Vec groupSize a))
    pairs = unconcatI mid

    res :: Vec n (Vec (2 * groupSize) a)
    res =  map (concat . func) pairs
        where
        func :: Vec 2 (Vec groupSize a) -> Vec 2 (Vec groupSize a)
        func (l@(_ :< toAdd) :> (xs :< toAddTo) :> Nil) = l :> (xs :< (toAdd `op` toAddTo)) :> Nil

    ress :: Vec ((2 * n) * groupSize) a
    ress = concat res

prefixSumWorkEfficient32 :: (a -> a -> a) -> Vec 32 a -> Vec 32 a
prefixSumWorkEfficient32 op vec 
    = prefixSumWorkEfficientStepB (SNat @ 1) op
    $ prefixSumWorkEfficientStepB (SNat @ 2) op
    $ prefixSumWorkEfficientStepB (SNat @ 4) op
    $ prefixSumWorkEfficientStepB (SNat @ 8) op
    $ prefixSumWorkEfficientStepA (SNat @ 4) op
    $ prefixSumWorkEfficientStepA (SNat @ 3) op
    $ prefixSumWorkEfficientStepA (SNat @ 2) op
    $ prefixSumWorkEfficientStepA (SNat @ 1) op
    $ prefixSumWorkEfficientStepA (SNat @ 0) op
    $ vec

