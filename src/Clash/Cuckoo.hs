{-# LANGUAGE RecordWildCards, StandaloneDeriving, DeriveAnyClass, DeriveGeneric #-}

{-| Implements the lookup side of a <https://en.wikipedia.org/wiki/Cuckoo_hashing Cuckoo hashtable>.

    Cuckoo hashtables are well suited to communicating key value pairs with hardware where the space of keys is large.
 -}
module Clash.Cuckoo (
    TableEntry(..),
    cuckoo',
    cuckoo,
    cuckooWithInsert
    ) where

import GHC.Generics
import Clash.Prelude
import Clash.Prelude.Moore
import Data.Maybe

data TableEntry k v = TableEntry {
    key   :: k,
    value :: v
} deriving (Generic)

deriving instance (BitPack k, BitPack v, KnownNat (BitSize v)) => BitPack (TableEntry k v)

{-| The lookup side of a Cuckoo hashtable. Uses split tables. Allows the hashes to be computed in advance, possibly over multiple cycles. -}
cuckoo' 
    :: forall dom gated sync m n k v. (HiddenClockReset dom gated sync, KnownNat m, KnownNat n, Eq k)
    => Vec (m + 1) (Signal dom (Maybe (Unsigned n, Maybe (TableEntry k v)))) -- ^ Table updates from software. A vector of (row number, row value) pair updates, one for each table. 
    -> Vec (m + 1) (Signal dom (Unsigned n))                                 -- ^ Vector of hashed keys, one for each table. CRCs make good hash functions.
    -> Signal dom k                                                          -- ^ The key to lookup.
    -> Signal dom (Maybe (Index (m + 1), Unsigned n, v))                     -- ^ The result of the lookup. Will be ready one cycle after the key is input. The table index and hash of the matching entry are also returned so that future table updates can be made from hardware.
cuckoo' tableUpdates hashes lookupKey = fold (liftA2 (<|>)) candidates
    where

    --Do the lookups
    fromMem :: Vec (m + 1) (Signal dom (Maybe (TableEntry k v)))
    fromMem = zipWith (blockRamPow2 (repeat Nothing)) hashes tableUpdates

    --Check if each item returned from memory matches
    candidates :: Vec (m + 1) (Signal dom (Maybe (Index (m + 1), Unsigned n, v)))
    candidates = izipWith (\idx -> liftA3 (checkCandidate idx) (register (errorX "key") lookupKey)) (map (register (errorX "initial hashes")) hashes) fromMem 
        where
        --Get rid of signals
        checkCandidate :: Index (m + 1) -> k -> Unsigned n -> Maybe (TableEntry k v) -> Maybe (Index (m + 1), Unsigned n, v)
        checkCandidate _   lookupKey _    Nothing = Nothing
        checkCandidate idx lookupKey hash (Just TableEntry{..}) 
            | lookupKey == key = Just (idx, hash, value)
            | otherwise        = Nothing

{-| The lookup side of a Cuckoo hashtable. Uses split tables. -}
cuckoo 
    :: forall dom gated sync m n k v. (HiddenClockReset dom gated sync, KnownNat m, KnownNat n, Eq k)
    => (k -> Vec (m + 1) (Unsigned n))                                       -- ^ Vector of hash functions, one for each table. CRCs make good hash functions.
    -> Vec (m + 1) (Signal dom (Maybe (Unsigned n, Maybe (TableEntry k v)))) -- ^ Table updates from software. A vector of (row number, row value) pair updates, one for each table. 
    -> Signal dom k                                                          -- ^ The key to lookup.
    -> Signal dom (Maybe (Index (m + 1), Unsigned n, v))                     -- ^ The result of the lookup. Will be ready one cycle after the key is input. The table index and hash of the matching entry are also returned so that future table updates can be made from hardware.
cuckoo hashFunctions tableUpdates lookupKey = cuckoo' tableUpdates hashes lookupKey
    where
    --compute all the hashes
    hashes :: Vec (m + 1) (Signal dom (Unsigned n))
    hashes = sequenceA $ hashFunctions <$> lookupKey

cuckooWithInsert
    :: forall dom gated sync m n k v. (HiddenClockReset dom gated sync, KnownNat m, KnownNat n, Eq k)
    => (k -> Vec (m + 1) (Unsigned n))                                       
    -> Signal dom k                                                          
    -> Signal dom v
    -> Signal dom Bool
    -> (
        Signal dom (Maybe (Index (m + 1), Unsigned n, v)), -- Table index, table row, value
        Signal dom Bool, 
        Signal dom Bool
        )
cuckooWithInsert hashFunctions key' value' insert = (lookupResult, isJust <$> writebackStage, insertingDone)
    where

    --------------------------------------------------------------------------------
    --Insertion logic
    --------------------------------------------------------------------------------

    --Insert/modify/delete begin by looking up the entry
    lookupStage :: Signal dom (TableEntry k v)
    lookupStage =  mux
        ((isJust <$> writebackStage) .&&. not <$> insertingDone)
        evictedEntry
        (TableEntry <$> key' <*> value')

    writebackStage :: Signal dom (Maybe (TableEntry k v))
    writebackStage =  medvedevB step Nothing (insert, insertingDone, lookupStage)
        where
        step Nothing  (True,   _,     lookupStage) = Just lookupStage
        step Nothing  (False,  _,     _)           = Nothing
        step (Just _) (_,      True,  _)           = Nothing
        step (Just _) (_,      False, lookupStage) = Just lookupStage

    insertingDone :: Signal dom Bool
    insertingDone =  (isJust <$> freeSlot) .||. (firstInsertCycle .&&. isJust <$> lookupResult)

    --Find a free slot to replace, if any
    freeSlot :: Signal dom (Maybe (Index (m + 1)))
    freeSlot =  findIndex isNothing <$> sequenceA fromMem
    --freeSlot =  fmap (join . find isJust) $ rotateLeft <$> (imap func <$> sequenceA fromMem) <*> toEvict
    --    where
    --    func idx Nothing = Just idx
    --    func _   _       = Nothing

    --Freerunning counter for randomly choosing table to evict from
    toEvict :: Signal dom (Index (m + 1))
    toEvict =  register 0 $ func <$> toEvict
        where
        func x
            | x == maxBound = 0
            | otherwise     = x + 1

    --Select the table entry to evict
    --This will become the value to insert in the next cycle
    --TODO: figure out how to get rid of fromJust 
    evictedEntry :: Signal dom (TableEntry k v)
    evictedEntry =  fmap fromJust $ (!!) <$> sequenceA fromMem <*> toEvict

    --Track if this is the first insert writeback cycle. If it is we must consider the case where the key was already in the hashtable
    firstInsertCycle = register False $ insert .&&. ((isNothing <$> writebackStage) .||. insertingDone)

    --Calculate the index to update. Prefer free slot, otherwise evict.
    indexToUpdate :: Signal dom (Index (m + 1))
    indexToUpdate =  selectIndex <$> firstInsertCycle <*> lookupResult <*> freeSlot <*> toEvict
        where
        selectIndex True (Just (idx, _, _)) _               _       = idx      --We are modifying an existing entry
        selectIndex _    _                  (Just freeSlot) _       = freeSlot --We found a free slot
        selectIndex _    _                  _               toEvict = toEvict  --No free slots, so we are evicting

    --Save the hashes of the evicted entry
    hashesD :: Vec (m + 1) (Signal dom (Unsigned n))
    hashesD =  map (register (errorX "initial registered hashes")) hashes

    --Update the chosen index if we are inserting with either the data to insert or the previously evicted table entry
    tableUpdates :: Vec (m + 1) (Signal dom (Maybe (Unsigned n, Maybe (TableEntry k v))))
    tableUpdates =  map (\idx -> genTableUpdate idx <$> indexToUpdate <*> writebackStage <*> sequenceA hashesD) $ iterateI (+1) 0
        where
        genTableUpdate
            :: Index (m + 1) 
            -> Index (m + 1) 
            -> Maybe (TableEntry k v)
            -> Vec (m + 1) (Unsigned n)
            -> Maybe (Unsigned n, Maybe (TableEntry k v))
        genTableUpdate currIdx idxToUpdate toInsert tableRow
            | Just toInsert <- toInsert, currIdx == idxToUpdate = Just (tableRow !! currIdx, Just toInsert)
            | otherwise                                         = Nothing

    --------------------------------------------------------------------------------
    --This stuff is common to lookups, deletes and inserts
    --------------------------------------------------------------------------------

    --Calculate the lookup hashes
    hashes :: Vec (m + 1) (Signal dom (Unsigned n))
    hashes =  sequenceA $ hashFunctions . key <$> lookupStage

    --Do the lookups
    fromMem :: Vec (m + 1) (Signal dom (Maybe (TableEntry k v)))
    fromMem =  zipWith (blockRamPow2 (repeat Nothing)) hashes tableUpdates

    --Check if each item returned from memory matches
    candidates :: Vec (m + 1) (Signal dom (Maybe (Index (m + 1), Unsigned n, v)))
    candidates =  izipWith (\idx -> liftA3 (checkCandidate idx) (register (errorX "key") key')) (map (register (errorX "initial hashes")) hashes) fromMem 
        where
        --Get rid of signals
        checkCandidate :: Index (m + 1) -> k -> Unsigned n -> Maybe (TableEntry k v) -> Maybe (Index (m + 1), Unsigned n, v)
        checkCandidate _   lookupKey _    Nothing = Nothing
        checkCandidate idx lookupKey hash (Just TableEntry{..}) 
            | lookupKey == key = Just (idx, hash, value)
            | otherwise        = Nothing

    lookupResult :: Signal dom (Maybe (Index (m + 1), Unsigned n, v))
    lookupResult =  fold (liftA2 (<|>)) candidates

