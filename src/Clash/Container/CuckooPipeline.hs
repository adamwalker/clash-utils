{-# LANGUAGE TemplateHaskell #-}

{-| Another <https://en.wikipedia.org/wiki/Cuckoo_hashing Cuckoo hashtable>.

    Cuckoo hashtables are well suited to communicating key value pairs with hardware where the space of keys is large.

    __FPGA proven__
 -}
module Clash.Container.CuckooPipeline (
    TableEntry(..),
    cuckooStage,
    cuckooPipeline,
    cuckooPipelineInsert',
    cuckooPipelineInsert,
    cuckooPipelineMultiMap,
    exampleDesign
    ) where

import Clash.Prelude
import GHC.Generics
import Data.Maybe

import Clash.ErrorControl.CRC

data TableEntry k v = TableEntry {
    key   :: k,
    value :: v
} deriving (Show, Generic, ShowX, NFDataX)

{-| One stage in the cuckoo pipeline
    Each stage manages its own table and has its own hash function
-}
cuckooStage 
    :: forall dom n numRamPipes k v. (HiddenClockResetEnable dom, KnownNat n, Eq k, NFDataX k, NFDataX v)
    => SNat numRamPipes                      -- ^ Number of ram read pipeline stages
    -> (k -> Unsigned n)                     -- ^ Hash function for this stage
    -> Signal dom k                          -- ^ Key to be looked up. For modifications and deletes, set this input
                                             --   in addition to the "modification" field below
    -> Signal dom k                          -- ^ Delayed key
    -> Signal dom (Maybe (TableEntry k v))   -- ^ Value to be inserted
    -> Signal dom (Maybe (Maybe v))          -- ^ Modification. "Nothing" is no modification. "Just Nothing" is delete.
    -> Signal dom (Maybe (TableEntry k v))   -- ^ Incoming value to be evicted fro mthe previous stage in the pipeline.
    -> (
        Signal dom (Maybe (TableEntry k v)), --Outgoing value to be evicted
        Signal dom (Maybe v),                --Lookup result
        Signal dom Bool                      --Busy
        )                                    -- ^ (Outgoing value to be evicted, Lookup result, Busy)
cuckooStage SNat hashFunc toLookup keyD insertVal modificationD incomingEviction = (
        mux (last evictingD) fromMem (pure Nothing), 
        luRes,
        (isJust <$> incomingEviction) .||. readPipelineBusy
    )
    where

    --Check if we have stuff in the read pipeline and therefore may get evictions in future cycles
    readPipelineBusy = foldl (.||.) (pure False) (init evictingD)

    --Choose what to insert
    --Prefer the incoming eviction from the previous stage, otherwise it will get dropped
    muxedIncoming :: Signal dom (Maybe (TableEntry k v))
    muxedIncoming = liftA2 (<|>) incomingEviction insertVal
    
    --Choose what to hash
    --Prefer the incoming eviction/insert to the key to be looked up
    hash :: Signal dom (Unsigned n)
    hash = hashFunc <$> liftA2 fromMaybe toLookup (fmap key <$> muxedIncoming)

    --Generate the write for inserts and incoming evictions from the previous stage
    writeCmd :: Signal dom (Maybe (Unsigned n, Maybe (TableEntry k v)))
    writeCmd =  func <$> hash <*> muxedIncoming 
        where
        func hash muxedIncoming = (hash, )  <$> (Just <$> muxedIncoming)

    --The block ram that stores this pipeline's table
    fromMem :: Signal dom (Maybe (TableEntry k v))
    fromMem =  last $ iterate (SNat @(numRamPipes + 1)) (register Nothing) 
        $ blockRamPow2 (repeat Nothing) hash (liftA2 (<|>) writeCmd finalMod)

    --Keep track of whether we are evicting
    --If so, on the next cycle, we need to pass the evicted entry onwards
    evictingD :: Vec (numRamPipes + 1) (Signal dom Bool)
    evictingD = generate (SNat @(numRamPipes + 1)) (register False) 
        $ isJust <$> muxedIncoming

    --
    --Lookup logic
    --

    --Compare the lookup key with the key in the table to see if we got a match
    luRes    :: Signal dom (Maybe v)
    luRes    =  checkCandidate <$> keyD <*> fromMem
        where
        checkCandidate keyD Nothing = Nothing
        checkCandidate keyD (Just res)
            | key res == keyD = Just $ value res
            | otherwise       = Nothing

    --
    --Modification logic
    --

    --Save the hash for the next cycle when we write back the modified value
    --to avoid recomputing it
    hashD :: Signal dom (Unsigned n)
    hashD =  last $ generate (SNat @(numRamPipes + 1)) (delay (errorX "Cuckoo: initial hashD")) hash

    --Calculate the block ram write back if we're doing a modification
    finalMod      :: Signal dom (Maybe (Unsigned n, Maybe (TableEntry k v)))
    finalMod      =  func <$> modificationD <*> (isJust <$> luRes) <*> hashD <*> keyD
        where
        func (Just mod) True hashD keyD = Just (hashD, TableEntry keyD <$> mod)
        func _    _    _           _    = Nothing

{-| Tie together M pipelineStages to build a full cuckoo pipeline
    Assumes that inserts are known to not be in the table
-}
cuckooPipeline 
    :: forall dom m n numRamPipes k v. (HiddenClockResetEnable dom, KnownNat n, Eq k, KnownNat m, NFDataX k, NFDataX v)
    => SNat numRamPipes                                  -- ^ Number of ram read pipeline stages
    -> Vec (m + 1) (k -> Unsigned n)                     -- ^ Hash functions for each stage
    -> Signal dom k                                      -- ^ Key to lookup or modify
    -> Vec (m + 1) (Signal dom (Maybe (Maybe v)))        -- ^ Modification. Nothing == no modification. Just Nothing == delete. Just (Just X) == overwrite with X
    -> Vec (m + 1) (Signal dom (Maybe (TableEntry k v))) -- ^ Vector of inserts. If you know a key is not in the table, you can insert it in any idle pipeline stage.
    -> (
        Vec (m + 1) (Signal dom (Maybe v)),              -- Lookup result
        Vec (m + 1) (Signal dom Bool)                    -- Vector of pipeline busy signals
        )                                                -- ^ (Lookup result, Vector of pipeline busy signals)
cuckooPipeline numRamPipes@SNat hashFuncs toLookup modificationsD inserts = (lookupResults, busys)
    where

    --Save the lookup key for comparison with the table entry key in the next cycle
    keyD :: Signal dom k
    keyD =  last $ generate (SNat @(numRamPipes + 1)) (delay (errorX "Cuckoo: initial keyD")) toLookup

    --Construct the pipeline
    (accum, res) = mapAccumL func accum $ zip3 hashFuncs inserts modificationsD
        where
        func accum (hashFunc, insert, modificationD) = 
            let (toEvict, lookupResult, insertBusy) = cuckooStage numRamPipes hashFunc toLookup keyD insert modificationD accum
            in  (toEvict, (lookupResult, insertBusy))

    --Combine the results and busy signals from the individual pipelines
    (lookupResults, busys) = unzip res

-- | Same as `cuckooPipelineInsert` but the insert/modification value is supplied on the second cycle
cuckooPipelineInsert'
    :: forall dom m n numRamPipes k v. (HiddenClockResetEnable dom, KnownNat n, Eq k, KnownNat m, NFDataX k, NFDataX v)
    => SNat numRamPipes              -- ^ Number of ram read pipeline stages
    -> Vec (m + 1) (k -> Unsigned n) -- ^ Hash functions for each stage
    -> Signal dom k                  -- ^ Key to lookup, modify or insert
    -> Signal dom (Maybe (Maybe v))  -- ^ Modification. Nothing == no modification. Just Nothing == delete. Just (Just X) == insert or overwrite existing value at key
    -> (
        Signal dom (Maybe v), -- Lookup result
        Signal dom Bool       -- Combined busy signal
        )                            -- ^ (Lookup result, Combined busy signal)
cuckooPipelineInsert' numRamPipes@SNat hashFuncs toLookup modificationD = (luRes, or <$> sequenceA busys)
    where

    --Instantiate the pipeline
    --Modifications are passed straight in. This works fine for inserts since none of the stages will match and there will be no writeback.
    --If none of the stages matched, then we insert into the first pipeline stage (only) on the next cycle.
    (lookupResults, busys) = cuckooPipeline numRamPipes hashFuncs toLookup (repeat modificationD) 
        $  insertD
        :> repeat (pure Nothing)

    toLookupD = last $ generate (SNat @(numRamPipes + 1)) (delay (errorX "Cuckoo: toLookup")) toLookup
    luRes     = fold (liftA2 (<|>)) lookupResults

    --Form an insert from our modification in case the key is not found in any tables
    insertD :: Signal dom (Maybe (TableEntry k v))
    insertD =  func <$> toLookupD <*> modificationD <*> (isJust <$> luRes)
        where
        func key (Just (Just value)) False = Just $ TableEntry key value
        func _   _                   _     = Nothing


{-| Convenience wrapper for cuckooPipeline that checks whether keys are present before inserting them. If found, it does a modification instead.
  | Only allows one insertion at a time so it is less efficient for inserts than `cuckooPipeline`
-}
cuckooPipelineInsert
    :: forall dom m n numRamPipes k v. (HiddenClockResetEnable dom, KnownNat n, Eq k, KnownNat m, NFDataX k, NFDataX v)
    => SNat numRamPipes              -- ^ Number of ram read pipeline stages
    -> Vec (m + 1) (k -> Unsigned n) -- ^ Hash functions for each stage
    -> Signal dom k                  -- ^ Key to lookup, modify or insert
    -> Signal dom (Maybe (Maybe v))  -- ^ Modification. Nothing == no modification. Just Nothing == delete. Just (Just X) == insert or overwrite existing value at key
    -> (
        Signal dom (Maybe v), -- Lookup result
        Signal dom Bool       -- Combined busy signal
        )                            -- ^ (Lookup result, Combined busy signal)
cuckooPipelineInsert numRamPipes@SNat hashFuncs toLookup modification = (luRes, busy)
    where
    (luRes, busy') = cuckooPipelineInsert' numRamPipes hashFuncs toLookup (last modificationD)
    busy           = (or . map isJust <$> sequenceA modificationD) .||. busy'
    modificationD  = generate (SNat @(numRamPipes + 1)) (register Nothing) $ mux busy (pure Nothing) modification

{-| Convenience wrapper for cuckooPipeline.
  | Implements a multimap with a bounded number of values
  | FPGA proven, but doesn't have randomised tests yet
-}
cuckooPipelineMultiMap
    :: forall dom m n numRamPipes k i v. (HiddenClockResetEnable dom, KnownNat n, Eq k, Eq i, KnownNat m, NFDataX k, NFDataX v, NFDataX i)
    => SNat numRamPipes                 -- ^ Number of ram read pipeline stages
    -> Vec (m + 1) (k -> Unsigned n)    -- ^ Hash functions for each stage
    -> Signal dom k                     -- ^ Key to lookup, modify or insert
    -> Signal dom (Maybe (i, Maybe v))  -- ^ Modification. Nothing == no modification. Just Nothing == delete. Just (Just X) == insert or overwrite existing value at key
    -> (
        Vec (m + 1) (Signal dom (Maybe (i, v))), -- Lookup results
        Signal dom Bool                          -- Combined busy signal
        )                               -- ^ (Lookup results, combined busy signal)
cuckooPipelineMultiMap numRamPipes@SNat hashFuncs toLookup modification = (lookupResults, busy)
    where

    --Instantiate the pipeline
    (lookupResults, busys) = cuckooPipeline numRamPipes hashFuncs toLookup modificationsD
        $  insertD
        :> repeat (pure Nothing)

    --We are busy the cycle after a modification, or if an insert is in progress
    busy           = (or . map isJust <$> sequenceA modificationD) .||. (or <$> sequenceA busys)
    --Save the modification for writeback on the next cycle
    modificationD  = generate (SNat @(numRamPipes + 1)) (register Nothing) $ mux busy (pure Nothing) modification
    --Save the lookup key for comparison on the next cycle
    toLookupD      = last $ generate (SNat @(numRamPipes + 1)) (delay (errorX "toLookupD")) toLookup

    --Calculate the modification/delete writebacks for each table
    modificationsD :: Vec (m + 1) (Signal dom (Maybe (Maybe (i, v))))
    modificationsD = map (liftA2 func (last modificationD)) lookupResults
        where
        func :: Maybe (i, Maybe v) -> Maybe (i, v) -> Maybe (Maybe (i, v))
        func (Just (modIdx, modValue)) (Just (tableIdx, tableValue))
            | modIdx == tableIdx = Just $ (modIdx, ) <$> modValue
            | otherwise          = Nothing
        func _                         _ = Nothing

    --Form an insert from our modification in case the key is not found in any tables
    insertD :: Signal dom (Maybe (TableEntry k (i, v)))
    insertD =  func <$> toLookupD <*> (last modificationD) <*> (all isNothing <$> sequenceA modificationsD)
        where
        func key (Just (idxUpdate, Just valueUpdate)) True
            = Just $ TableEntry key (idxUpdate, valueUpdate)
        func _ _ _
            = Nothing

-- | An example cuckoo hashtable top level design
exampleDesign
    :: HiddenClockResetEnable dom
    => Signal dom (BitVector 64) -- ^ Key to lookup/modify/delete
    -> Signal dom Bool           -- ^ Perform modification
    -> Signal dom Bool           -- ^ Modification is delete
    -> Signal dom (BitVector 32) -- ^ New value for modifications
    -> (
        Signal dom (Maybe (BitVector 32)),
        Signal dom Bool
        )                        -- ^ (Lookup result, busy)
exampleDesign lu modify delete val = cuckooPipelineInsert (SNat @0) hashFunctions lu (cmd <$> modify <*> delete <*> val)
    where
    cmd modify delete val
        | modify && delete = Just Nothing
        | modify           = Just $ Just val
        | otherwise        = Nothing
    hashFunctions 
        =  (\x -> unpack (slice d9  d0  $ crc x))
        :> (\x -> unpack (slice d19 d10 $ crc x))
        :> (\x -> unpack (slice d29 d20 $ crc x))
        :> Nil
        where
        table = $(lift $ (makeCRCTable (pack . crcSteps crc32Poly (repeat False)) :: Vec 64 (BitVector 32))) 
        crc   = crcTable table 

