-- | Utility functions for wiring up streams with backpressure
module Clash.Stream.Chain (
    chainStream,
    VecStream,
    chainStreamVec
    ) where

import Clash.Prelude

-- | Convenience function for chaining streams together, correctly connecting up their ready signals
chainStream 
    :: (Signal dom b -> Signal dom Bool -> (Signal dom a, Signal dom Bool)) -- ^ Regular stream function accepting and returning backpressure
    -> (Signal dom Bool -> Signal dom b)                                    -- ^ Stream function earlier in the pipeline
    -> (Signal dom Bool -> Signal dom a)                                    -- ^ Output stream function
chainStream downstream upstream readyOut = streamOut
    where
    dat                = upstream ready
    (streamOut, ready) = downstream dat readyOut

type VecStream a b dom m
    =  Signal dom Bool               -- ^ Downstream ready
    -> Vec m (Signal dom a)          -- ^ Incoming streams
    -> (
            Vec m (Signal dom Bool),
            Signal dom b
       )                             -- ^ (Upstream readys, Outgoing streams)

-- | Convenience function for merging streams together, correctly connecting up their ready signals
chainStreamVec
    :: VecStream a b dom m                     -- ^ Regular stream function, accepting and returning backpressure, operating on a vector of streams
    -> Vec m (Signal dom Bool -> Signal dom a) -- ^ Stream functions earlier in the pipeline
    -> (Signal dom Bool -> Signal dom b)       -- ^ Output stream function
chainStreamVec op streams readyOut = out
    where
    (readys, out) = op readyOut $ zipWith ($) streams readys

