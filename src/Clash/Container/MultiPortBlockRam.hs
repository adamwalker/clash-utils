module Clash.Container.MultiPortBlockRam (
        multiPortBlockRam
    ) where

import Clash.Prelude

multiPortBlockRam 
    :: forall dom nWrites nWrites' nReads n addrBits
    .  (HiddenClockResetEnable dom, nWrites ~ (nWrites' + 1), KnownNat n, KnownNat addrBits, KnownNat nWrites)
    => Vec nReads (Signal dom (Unsigned addrBits))
    -> Vec nWrites (Signal dom (Maybe (Unsigned addrBits, BitVector n)))
    -> Vec nReads (Signal dom (BitVector n))
multiPortBlockRam reads writes = readReads
    where

    --Writeback state machine
    memWrites :: Vec nWrites (Signal dom (Maybe (Unsigned addrBits, BitVector n)))
    memWrites =  zipWith (liftA2 func) writeReads $ map (register Nothing) writes
        where
        func 
            :: BitVector n 
            -> Maybe (Unsigned addrBits, BitVector n) 
            -> Maybe (Unsigned addrBits, BitVector n)
        func x = fmap (fmap (xor x)) 

    --Initial read of other banks before write
    writeReadAddrs :: Vec nWrites (Signal dom (Unsigned addrBits))
    writeReadAddrs =  map (fmap func) writes
        where
        func Nothing            = 0
        func (Just (addr, dat)) = addr

    --Write bank
    --Inner dimension is for one write
    writeReads :: Vec nWrites (Signal dom (BitVector n))
    writeReads =  map (fold (liftA2 xor)) $ imap func0 writeReadAddrs
        where
        func0 
            :: Index nWrites 
            -> Signal dom (Unsigned addrBits) 
            -> Vec nWrites (Signal dom (BitVector n))
        func0 idx0 readAddr = imap func1 memWrites 
            where
            func1 
                :: Index nWrites 
                -> Signal dom (Maybe (Unsigned addrBits, BitVector n)) 
                -> Signal dom (BitVector n)
            func1 idx1 writeCmd
                | idx0 == idx1 = 0
                | otherwise    = readNew (blockRamPow2 (repeat 0)) readAddr writeCmd

    --Read bank
    readReads :: Vec nReads (Signal dom (BitVector n))
    readReads =  map (fold (liftA2 xor)) $ map func reads
        where
        func :: Signal dom (Unsigned addrBits) -> Vec nWrites (Signal dom (BitVector n))
        func readAddr = map (readNew (blockRamPow2 (repeat 0)) readAddr) memWrites

