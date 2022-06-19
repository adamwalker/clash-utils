module Clash.Container.PriorityQueue (
        prioQueue,
        PQ,
        prioQueueMultiRam
    ) where

import Clash.Prelude

import Clash.Misc
import Clash.Sort.Insertion

-- | A simple linear priority queue stored in block ram
prioQueue
    :: forall n dom iters a identBits
    .  (HiddenClockResetEnable dom, 1 <= ((2 ^ identBits) * iters), 1 <= iters, Bounded a, NFDataX a, KnownNat n, Ord a, KnownNat identBits)
    => SNat iters                       -- ^ Number of iterations to perform for a insert
    -> Signal dom (BitVector identBits) -- ^ Since multiple priority queues can be stored in a ram, this tells us which one to modify
    -> Signal dom Bool                  -- ^ Do an insert
    -> Signal dom Bool                  -- ^ Delete
    -> Signal dom a                     -- ^ Value to insert
    -> Signal dom a                     -- ^ Value to delete
    -> Signal dom a                     -- ^ Downstream cascade in for chaining multiple priority queues
    -> (
        (Signal dom (Vec (n + 1) a), Signal dom Bool), 
        (Signal dom Bool, Signal dom Bool, Signal dom a, Signal dom a, Signal dom (BitVector identBits))
        ) -- ^ ((Result, busy), cascade out)
prioQueue SNat ident insert delete val deleteVal readCascadeIn = (result, cascade)
    where

    ident'  = mux (insert .||. delete) ident identD
    identD  = delay 0                          ident' --TODO: why does this fail if the initial value is undefined
    identDD = delay (errorX "initial identDD") identD

    result = (readRes, inProgressInsert .||. inProgressDelete .||. inProgressDeleteD)
    cascade = (
            register False $ inProgressInsert .&&. progressCntr .==. pure maxBound, 
            register False $ inProgressDelete .&&. progressCntr .==. pure maxBound,
            next,
            deleteValSaved,
            identD
        )

    --Read/write addresses
    progressCntr :: Signal dom (Index iters)
    progressCntr = register 0 $ func <$> progressCntr <*> (insert .||. delete) 
        where
        func 0 False = 0
        func c _     
            | c == maxBound = 0
            | otherwise     = c + 1

    insertWriteAddress :: Signal dom (Index iters)
    insertWriteAddress =  delay (errorX "initial insertWriteAddress") progressCntr

    deleteWriteAddress :: Signal dom (Index iters)
    deleteWriteAddress =  delay (errorX "initial deleteWriteAddress") insertWriteAddress

    --Track operation in progress
    inProgressInsert :: Signal dom Bool
    inProgressInsert =  register False $ ((progressCntr .>. 0) .&&. inProgressInsert) .||. insert

    inProgressDelete :: Signal dom Bool
    inProgressDelete =  register False $ ((progressCntr .>. 0) .&&. inProgressDelete) .||. delete

    inProgressDeleteD :: Signal dom Bool
    inProgressDeleteD =  register False inProgressDelete

    --The block ram
    readRes :: Signal dom (Vec (n + 1) a)
    readRes 
        = blockRam1 
            NoClearOnReset 
            (SNat @((2 ^ identBits) * iters))
            (repeat maxBound :: Vec (n + 1) a) 
            (ident' ++## (pack <$> progressCntr)) 
            writeCommand

    --Writeback calculation for insert
    toInsert                    = delay (errorX "prioQueue: initial toInsert") $ mux insert val next
    (toWriteBackInsert :< next) = sequenceA $ sortedInsert <$> toInsert <*> readRes

    --Writeback calculation for delete
    readResD :: Signal dom (Vec (n + 1) a)
    readResD =  delay (repeat (errorX "initial readResD")) readRes

    deleteValSaved :: Signal dom a
    deleteValSaved =  delayEn (errorX "initial deleteValSaved") delete deleteVal

    deleteWriteBack :: Signal dom (Vec (n + 1) a)
    deleteWriteBack 
        =   func 
        <$> deleteValSaved
        <*> readResD 
        <*> mux (deleteWriteAddress .==. pure maxBound) readCascadeIn (head <$> readRes)
        where
        func x readResD h = sortedDelete x (readResD :< h)

    --The final writeback command
    writeCommand 
        =   func 
        <$> inProgressInsert 
        <*> identD
        <*> insertWriteAddress 
        <*> sequenceA toWriteBackInsert 
        <*> inProgressDeleteD 
        <*> identDD
        <*> deleteWriteAddress 
        <*> deleteWriteBack
        where
        func True identD  idx toInsert _    _       _   _        = Just (identD  ++# pack idx, toInsert)
        func _    _       _   _        True identDD idx toDelete = Just (identDD ++# pack idx, toDelete)
        func _    _       _   _        _    _       _   _        = Nothing


type PQ dom n a identBits
    =  Signal dom (BitVector identBits) -- ^ Queue identifier
    -> Signal dom Bool                  -- ^ Do an insert
    -> Signal dom Bool                  -- ^ Delete
    -> Signal dom a                     -- ^ Value to insert
    -> Signal dom a                     -- ^ Value to delete
    -> Signal dom a                     -- ^ Downstream cascade in for chaining multiple priority queues
    -> (
        (Signal dom (Vec (n + 1) a), Signal dom Bool), 
        (Signal dom Bool, Signal dom Bool, Signal dom a, Signal dom a, Signal dom (BitVector identBits))
        ) -- ^ ((Result, busy), cascade out)

prioQueueMultiRam
    :: forall n dom iters a identBits numStages
    .  (HiddenClockResetEnable dom, 1 <= iters, 1 <= ((2 ^ identBits) * iters), Bounded a, NFDataX a, KnownNat n, Ord a, KnownNat identBits)
    => SNat (numStages + 1)             -- ^ Number of stages to cascade
    -> SNat iters                       -- ^ Number of iterations to perform for a insert
    -> Signal dom (BitVector identBits) -- ^ Since multiple priority queues can be stored in a ram, this tells us which one to modify
    -> Signal dom Bool                  -- ^ Do an insert
    -> Signal dom Bool                  -- ^ Delete
    -> Signal dom a                     -- ^ Value to insert
    -> Signal dom a                     -- ^ Value to delete
    -> Signal dom a                     -- ^ Downstream cascade in for chaining multiple priority queues
    -> (
        (Signal dom (Vec (n + 1) a), Signal dom Bool), 
        (Signal dom Bool, Signal dom Bool, Signal dom a, Signal dom a, Signal dom (BitVector identBits))
        ) -- ^ ((Result, busy), cascade out)
prioQueueMultiRam numStages iters
    = foldl1 combineQueues (replicate numStages (prioQueue iters)) 
    where
    combineQueues
        :: forall dom n a
        .  PQ dom n a identBits
        -> PQ dom n a identBits
        -> PQ dom n a identBits
    combineQueues left right ident insert delete val deleteVal readCascadeIn
        = (res, cascadeOut)
        where
        (res, (insert', delete', val', deleteVal', ident'))   = left  ident  insert  delete  val  deleteVal  (head <$> res')
        ((res' :: Signal dom (Vec (n + 1) a), _), cascadeOut) = right ident' insert' delete' val' deleteVal' readCascadeIn

