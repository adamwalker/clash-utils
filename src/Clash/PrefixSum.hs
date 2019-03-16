module Clash.PrefixSum (
    prefixSumParallelStep,
    prefixSumParallel32
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
