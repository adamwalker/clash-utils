{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveAnyClass #-}

{-| Another <https://en.wikipedia.org/wiki/Cuckoo_hashing Cuckoo hashtable>.

    Cuckoo hashtables are well suited to communicating key value pairs with hardware where the space of keys is large.

    __FPGA proven__
 -}
module Clash.Container.CuckooPipeline (
    TableEntry(..),
    cuckooPipelineStage,
    cuckooPipeline,
    cuckooPipelineInsert,
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
cuckooPipelineStage 
    :: forall dom n k v. (HiddenClockResetEnable dom, KnownNat n, Eq k, NFDataX k, NFDataX v)
    => (k -> Unsigned n)                     -- ^ Hash function for this stage
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
cuckooPipelineStage hashFunc toLookup keyD insertVal modificationD incomingEviction = (
        mux evicting fromMem (pure Nothing), 
        luRes,
        isJust <$> incomingEviction
    )
    where

    --Choose what to insert
    --Prefer the incoming eviction from the previous stage, otherwise it will get dropped
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
    fromMem =  blockRamPow2 (repeat Nothing) hash (liftA2 (<|>) writeCmd finalMod)

    --Keep track of whether we are evicting
    --If so, on the next cycle, we need to pass the evicted entry onwards
    evicting :: Signal dom Bool
    evicting =  register False $ isJust <$> muxedIncoming

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
    hashD         :: Signal dom (Unsigned n)
    hashD         =  register (errorX "Cuckoo: initial hashD") hash

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
    :: forall dom m n k v. (HiddenClockResetEnable dom, KnownNat n, Eq k, KnownNat m, NFDataX k, NFDataX v)
    => Vec (m + 1) (k -> Unsigned n)                     -- ^ Hash functions for each stage
    -> Signal dom k                                      -- ^ Key to lookup or modify
    -> Signal dom (Maybe (Maybe v))                      -- ^ Modification. Nothing == no modification. Just Nothing == delete. Just (Just X) == overwrite with X
    -> Vec (m + 1) (Signal dom (Maybe (TableEntry k v))) -- ^ Vector of inserts. If you know a key is not in the table, you can insert it in any idle pipeline stage.
    -> (
        Signal dom (Maybe v),         -- Lookup result
        Vec (m + 1) (Signal dom Bool) -- Vector of pipeline busy signals
        )                                                -- ^ (Lookup result, Vector of pipeline busy signals)
cuckooPipeline hashFuncs toLookup modificationD inserts = (fold (liftA2 (<|>)) lookupResults, busys)
    where

    --Save the lookup key for comparison with the table entry key in the next cycle
    keyD :: Signal dom k
    keyD =  register (errorX "Cuckoo: initial keyD") toLookup

    --Construct the pipeline
    (accum, res) = mapAccumL func accum $ zip hashFuncs inserts
        where
        func accum (hashFunc, insert) = 
            let (toEvict, lookupResult, insertBusy) = cuckooPipelineStage hashFunc toLookup keyD insert modificationD accum
            in  (toEvict, (lookupResult, insertBusy))

    --Combine the results and busy signals from the individual pipelines
    (lookupResults, busys) = unzip res

{-| Convenince wrapper for the cuckooPipeline that checks whether keys are present before inserting them. If found, it does a modification instead.
  | Only allows one insertion at a time so it is less efficient for inserts than `cuckooPipeline`
-}
cuckooPipelineInsert
    :: forall dom m n k v. (HiddenClockResetEnable dom, KnownNat n, Eq k, KnownNat m, NFDataX k, NFDataX v)
    => Vec (m + 1) (k -> Unsigned n) -- ^ Hash functions for each stage
    -> Signal dom k                  -- ^ Key to lookup, modify or insert
    -> Signal dom (Maybe (Maybe v))  -- ^ Modification. Nothing == no modification. Just Nothing == delete. Just (Just X) == insert or overwrite existing value at key
    -> (
        Signal dom (Maybe v), -- Lookup result
        Signal dom Bool       -- Combined busy signal
        )                            -- ^ (Lookup result, Combined busy signal)
cuckooPipelineInsert hashFuncs toLookup modification = (luRes, busy)
    where

    --Instantiate the pipeline
    --Modifications are passed straight in. This works fine for inserts since none of the stages will match and there will be no writeback.
    --If none of the stages matched, then we insert into the first pipeline stage (only) on the next cycle.
    (luRes, busys) = cuckooPipeline hashFuncs toLookup modificationD 
        $  insertD
        :> repeat (pure Nothing)

    busy          = (isJust <$> modificationD) .||. (or <$> sequenceA busys)
    modificationD = register Nothing $ mux busy (pure Nothing) modification
    toLookupD     = register (errorX "toLookupD") toLookup

    --Form an insert from our modification in case the key is not found in any tables
    insertD :: Signal dom (Maybe (TableEntry k v))
    insertD =  func <$> toLookupD <*> modificationD <*> (isJust <$> luRes)
        where
        func key (Just (Just value)) False = Just $ TableEntry key value
        func _   _                   _     = Nothing

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
exampleDesign lu modify delete val = cuckooPipelineInsert hashFunctions lu (cmd <$> modify <*> delete <*> val)
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
        table = $(lift $ (makeCRCTable (pack . crcSteps crc32Poly (repeat 0)) :: Vec 64 (BitVector 32))) 
        crc   = crcTable table 

