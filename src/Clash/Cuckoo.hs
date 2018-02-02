{-# LANGUAGE RecordWildCards #-}
module Clash.Cuckoo (
    TableEntry(..),
    cuckoo
    ) where

import Clash.Prelude

data TableEntry k v = TableEntry {
    key   :: k,
    value :: v
}

cuckoo 
    :: forall dom gated sync m n k v. (HasClockReset dom gated sync, KnownNat n, Eq k)
    => Vec (m + 1) (k -> Unsigned n)
    -> Vec (m + 1) (Signal dom (Maybe (Unsigned n, Maybe (TableEntry k v))))
    -> Signal dom k
    -> Signal dom (Maybe v)
cuckoo hashFunctions tableUpdates lookupKey = fold (liftA2 (<|>)) candidates
    where

    --compute all the hashes
    readAddresses :: Vec (m + 1) (Signal dom (Unsigned n))
    readAddresses = map (($ lookupKey) . fmap) hashFunctions

    --Do the lookups
    fromMem :: Vec (m + 1) (Signal dom (Maybe (TableEntry k v)))
    fromMem = zipWith (blockRamPow2 (repeat Nothing)) readAddresses tableUpdates

    --Check if each item returned from memory matches
    candidates :: Vec (m + 1) (Signal dom (Maybe v))
    candidates = map (liftA2 checkCandidate (register (errorX "key") lookupKey)) fromMem
        where
        --Get rid of signals
        checkCandidate :: k -> Maybe (TableEntry k v) -> Maybe v
        checkCandidate lookupKey Nothing = Nothing
        checkCandidate lookupKey (Just TableEntry{..})
            | lookupKey == key = Just value
            | otherwise        = Nothing

