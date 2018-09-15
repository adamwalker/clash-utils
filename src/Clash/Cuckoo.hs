{-# LANGUAGE RecordWildCards, StandaloneDeriving, DeriveAnyClass, DeriveGeneric #-}

{-| Implements the lookup side of a <https://en.wikipedia.org/wiki/Cuckoo_hashing Cuckoo hashtable>.

    Cuckoo hashtables are well suited to communicating key value pairs with hardware where the space of keys is large.
 -}
module Clash.Cuckoo (
    TableEntry(..),
    cuckooLookup',
    cuckooLookup,
    cuckoo',
    cuckoo
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
cuckooLookup' 
    :: forall dom gated sync m n k v. (HiddenClockReset dom gated sync, KnownNat m, KnownNat n, Eq k)
    => Vec (m + 1) (Signal dom (Maybe (Unsigned n, Maybe (TableEntry k v)))) -- ^ Table updates from software. A vector of (row number, row value) pair updates, one for each table. 
    -> Vec (m + 1) (Signal dom (Unsigned n))                                 -- ^ Vector of hashed keys, one for each table. CRCs make good hash functions.
    -> Signal dom k                                                          -- ^ The key to lookup.
    -> Signal dom (Maybe (Index (m + 1), Unsigned n, v))                     -- ^ The result of the lookup. Will be ready one cycle after the key is input. The table index and hash of the matching entry are also returned so that future table updates can be made from hardware.
cuckooLookup' tableUpdates hashes lookupKey = fold (liftA2 (<|>)) candidates
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
cuckooLookup 
    :: forall dom gated sync m n k v. (HiddenClockReset dom gated sync, KnownNat m, KnownNat n, Eq k)
    => (k -> Vec (m + 1) (Unsigned n))                                       -- ^ Vector of hash functions, one for each table. CRCs make good hash functions.
    -> Vec (m + 1) (Signal dom (Maybe (Unsigned n, Maybe (TableEntry k v)))) -- ^ Table updates from software. A vector of (row number, row value) pair updates, one for each table. 
    -> Signal dom k                                                          -- ^ The key to lookup.
    -> Signal dom (Maybe (Index (m + 1), Unsigned n, v))                     -- ^ The result of the lookup. Will be ready one cycle after the key is input. The table index and hash of the matching entry are also returned so that future table updates can be made from hardware.
cuckooLookup hashFunctions tableUpdates lookupKey = cuckooLookup' tableUpdates hashes lookupKey
    where
    --compute all the hashes
    hashes :: Vec (m + 1) (Signal dom (Unsigned n))
    hashes = sequenceA $ hashFunctions <$> lookupKey

{-| Like `cuckoo` but allows hash functions which take multiple cycles -}
cuckoo'
    :: forall dom gated sync cnt m n k v. (HiddenClockReset dom gated sync, KnownNat m, KnownNat n, Eq k, KnownNat cnt)
    => Vec (m + 1) (Signal dom (Unsigned n)) -- ^ Vector of hashes, one for each table. CRCs make good hash functions.
    -> Signal dom k                          -- ^ Key to operate on (for lookups, insertions and deletions)           
    -> Signal dom v                          -- ^ Value to insert (for insertions only)
    -> Signal dom Bool                       -- ^ Insert. Insertions take a variable number of cycles.
    -> Signal dom Bool                       -- ^ Delete. Deletions take 3 cycles.
    -> Signal dom Bool                       -- ^ Asserted when hash computation for the evicted entry is complete
    -> (
        Signal dom (Maybe (Index (m + 1), Unsigned n, v)), -- Table index, table row, value
        Signal dom Bool, 
        Signal dom Bool,
        Signal dom (TableEntry k v),
        Signal dom Bool,
        Signal dom (Unsigned cnt)
        ) -- ^ A tuple: (Lookup result, inserting in progress, last insert cycle, evicted key to hash, hash request, number of iterations this insert took). The lookup result will be ready two cycles after the key is input. The table index and hash of the matching entry are also returned so that future table updates can be made from hardware.
cuckoo' hashes key' value' insert delete evictedHashesDone = (lookupResult, isJust <$> writebackStage, insertingDone, evictedEntry, hashRequest, insertIters)
    where

    --------------------------------------------------------------------------------
    --Stats
    --------------------------------------------------------------------------------
    insertIters = register 0 $ func <$> firstInsertCycle <*> (progressInsert .&&. isJust <$> writebackStage) <*> insertIters
        where
        func True  _    _ = 0
        func _     True i = i + 1
        func _     _    i = i

    --------------------------------------------------------------------------------
    --Insertion logic
    --------------------------------------------------------------------------------
    progressInsert :: Signal dom Bool
    progressInsert = register False $ register False $ evictedHashesDone .||. firstInsertCycle

    --------------------------------------------------------------------------------
    --Insertion logic
    --------------------------------------------------------------------------------
    hashRequest = (isJust <$> writebackStage) .&&. (not <$> insertingDone) .&&. progressInsert

    --Track if this is the first insert writeback cycle. If it is we must consider the case where the key was already in the hashtable
    firstInsertCycle   = insert .&&. ((isNothing <$> writebackStage) .||. insertingDone)
    firstInsertCycleD  = register False firstInsertCycle
    firstInsertCycleDD = register False firstInsertCycleD

    writebackStage :: Signal dom (Maybe (TableEntry k v))
    writebackStage =  medvedevB step Nothing (progressInsert, insert, insertingDone, evictedEntry, TableEntry <$> key' <*> value')
        where
        step Nothing   (_,      True,   _,     _,            insertEntry) = Just insertEntry   --Insert request and we are idle
        step Nothing   (_,      False,  _,     _,            _          ) = Nothing            --No requests and we are idle
        step (Just st) (False,  _,      _,     _,            _          ) = Just st            --We are evicting but the hashes aren't done yet
        step (Just _)  (_,      _,      True,  _,            _          ) = Nothing            --We are finished inserting
        step (Just _)  (_,      _,      False, evictedEntry, _          ) = Just evictedEntry  --We are evicting and the hashes are done but we didnt find a slot

    insertingDone :: Signal dom Bool
    insertingDone = progressInsert .&&. ((isJust <$> freeSlot) .||. (isJust <$> lookupResult .&&. firstInsertCycleDD))

    --Find a free slot to replace, if any
    freeSlot :: Signal dom (Maybe (Index (m + 1)))
    freeSlot =  findIndex isNothing <$> sequenceA fromMem
    --freeSlot =  fmap (join . find isJust) $ rotateLeft <$> (imap func <$> sequenceA fromMem) <*> toEvict
    --    where
    --    func idx Nothing = Just idx
    --    func _   _       = Nothing

    --Freerunning counter for randomly choosing table to evict from
    toEvict :: Signal dom (Index (m + 1))
    toEvict =  regEn 0 progressInsert $ func <$> toEvict
        where
        func x
            | x == maxBound = 0
            | otherwise     = x + 1

    --Select the table entry to evict
    --This will become the value to insert in the next cycle
    --TODO: figure out how to get rid of fromJust 
    evictedEntry :: Signal dom (TableEntry k v)
    evictedEntry =  fmap fromJust $ (!!) <$> sequenceA fromMem <*> toEvict

    --Calculate the index to update. Prefer free slot, otherwise evict.
    indexToUpdate :: Signal dom (Index (m + 1))
    indexToUpdate =  selectIndex <$> firstInsertCycleDD <*> lookupResult <*> freeSlot <*> toEvict
        where
        selectIndex True (Just (idx, _, _)) _               _       = idx      --We are modifying an existing entry
        selectIndex _    _                  (Just freeSlot) _       = freeSlot --We found a free slot
        selectIndex _    _                  _               toEvict = toEvict  --No free slots, so we are evicting

    deleteD  = register False delete
    deleteDD = register False deleteD

    --Save the hashes of the evicted entry
    hashesD :: Vec (m + 1) (Signal dom (Unsigned n))
    hashesD =  map (register (errorX "initial registered hashes")) hashes

    hashesDD :: Vec (m + 1) (Signal dom (Unsigned n))
    hashesDD =  map (register (errorX "initial registered hashes")) hashesD

    --Update the chosen index if we are inserting with either the data to insert or the previously evicted table entry
    tableUpdates :: Vec (m + 1) (Signal dom (Maybe (Unsigned n, Maybe (TableEntry k v))))
    tableUpdates =  map (\idx -> 
        genTableUpdate idx 
            <$> indexToUpdate 
            <*> writebackStage 
            <*> deleteDD
            <*> lookupResult
            <*> sequenceA hashesDD
            <*> progressInsert
            ) $ iterateI (+1) 0
        where
        genTableUpdate
            :: Index (m + 1) 
            -> Index (m + 1) 
            -> Maybe (TableEntry k v)
            -> Bool
            -> Maybe (Index (m + 1), Unsigned n, v)
            -> Vec (m + 1) (Unsigned n)
            -> Bool
            -> Maybe (Unsigned n, Maybe (TableEntry k v))
        genTableUpdate currIdx idxToUpdate toInsert deleting lookupResult tableRow hashesReady

            | hashesReady
            , Just toInsert <- toInsert
            , currIdx == idxToUpdate 
            = Just (tableRow !! currIdx, Just toInsert)

            | deleting
            , Just (foundIndex, foundRow, _) <- lookupResult
            , currIdx == foundIndex 
            = Just (foundRow, Nothing)

            | otherwise 
            = Nothing

    --------------------------------------------------------------------------------
    --This stuff is common to lookups, deletes and inserts
    --------------------------------------------------------------------------------

    --Do the lookups
    fromMem :: Vec (m + 1) (Signal dom (Maybe (TableEntry k v)))
    fromMem =  map (register undefined) $ zipWith (blockRamPow2 (repeat Nothing)) hashes tableUpdates

    keyD :: Signal dom k
    keyD =  register (errorX "key") key'

    keyDD :: Signal dom k
    keyDD =  register (errorX "key") keyD

    --Check if each item returned from memory matches
    candidates :: Vec (m + 1) (Signal dom (Maybe (Index (m + 1), Unsigned n, v)))
    candidates =  izipWith (\idx -> liftA3 (checkCandidate idx) keyDD) hashesDD fromMem 
        where
        --Get rid of signals
        checkCandidate :: Index (m + 1) -> k -> Unsigned n -> Maybe (TableEntry k v) -> Maybe (Index (m + 1), Unsigned n, v)
        checkCandidate _   lookupKey _    Nothing = Nothing
        checkCandidate idx lookupKey hash (Just TableEntry{..}) 
            | lookupKey == key = Just (idx, hash, value)
            | otherwise        = Nothing

    lookupResult :: Signal dom (Maybe (Index (m + 1), Unsigned n, v))
    lookupResult =  fold (liftA2 (<|>)) candidates

{-| A full Cuckoo hashtable supporting lookups, modification, insertion and deletion -}
cuckoo
    :: forall dom gated sync cnt m n k v. (HiddenClockReset dom gated sync, KnownNat m, KnownNat n, Eq k, KnownNat cnt)
    => (k -> Vec (m + 1) (Unsigned n)) -- ^ Vector of hash functions, one for each table. CRCs make good hash functions.
    -> Signal dom k                    -- ^ Key to operate on (for lookups, insertions and deletions)
    -> Signal dom v                    -- ^ Value to insert (for insertions only)
    -> Signal dom Bool                 -- ^ Insert. Insertions take a variable number of cycles.
    -> Signal dom Bool                 -- ^ Delete. Deletions take 3 cycles.
    -> (
        Signal dom (Maybe (Index (m + 1), Unsigned n, v)), -- Table index, table row, value
        Signal dom Bool, 
        Signal dom Bool,
        Signal dom (Unsigned cnt)
        ) -- ^ A tuple: (Lookup result, inserting in progress, last insert cycle, number of iterations this insert took). The lookup result will be ready two cycles after the key is input. The table index and hash of the matching entry are also returned so that future table updates can be made from hardware.
cuckoo hashFunctions key' value' insert delete = (lookupResult, inserting, insertingDone, insertIters)
    where

    --Mux the key to lookup
    hashRequestD :: Signal dom Bool
    hashRequestD = register False hashRequest

    keyD :: Signal dom k
    keyD =  regEn (errorX "initial key") hashRequest $ key <$> evictedEntry

    toHash :: Signal dom k
    toHash =  mux
        hashRequestD
        keyD
        key'

    --Calculate the lookup hashes
    hashes :: Vec (m + 1) (Signal dom (Unsigned n))
    hashes =  sequenceA $ hashFunctions <$> toHash

    (lookupResult, inserting, insertingDone, evictedEntry, hashRequest, insertIters) = cuckoo' hashes key' value' insert delete hashRequestD


