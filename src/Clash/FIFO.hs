{-# LANGUAGE ScopedTypeVariables #-}

{-| A block ram FIFO -}
module Clash.FIFO (
    blockRamFIFO,
    blockRamFIFOMaybe
    ) where

import Clash.Prelude

import Data.Maybe

{-| A FIFO backed by block ram. The input does not fall through, i.e. if the FIFO is empty and a value is written to it, that value is not available on the output in the same cycle. A current limitation of this FIFO is that its capacity is one less than the capacity of the underlying block ram. -}
blockRamFIFO 
    :: forall dom gated sync size a. (HiddenClockReset dom gated sync, KnownNat size, Undefined a)
    => SNat size   -- ^ FIFO size
    -> Signal dom Bool -- ^ Read request
    -> Signal dom a    -- ^ Write data
    -> Signal dom Bool -- ^ Write request
    -> (
        Signal dom a,    -- Read data
        Signal dom Bool, -- Empty
        Signal dom Bool  -- Full
    )
blockRamFIFO size rReq wData wReq = (ramOut, empty, full)
    where
    --The backing ram
    ramOut = blockRam (repeat (errorX "Initial FIFO ram contents") :: Vec size a) rAddr' (mux wEn (Just <$> bundle (wAddr, wData)) (pure Nothing))
    --The status signals
    empty  = (register 0 wAddr) .==. rAddr
    full   = rAddr .==. (wrappingInc <$> wAddr)
    --Only write if there is space
    wEn    = wReq .&&. fmap not full
    rEn    = rReq .&&. fmap not empty
    --The read and write pointers
    wAddr, rAddr :: Signal dom (Index size)
    wAddr  = register 0 $ mux wEn (wrappingInc <$> wAddr) wAddr
    rAddr' = mux rEn (wrappingInc <$> rAddr) rAddr
    rAddr  = register 0 rAddr'

    wrappingInc :: Index size -> Index size
    wrappingInc val
        | val == maxBound = 0
        | otherwise       = val + 1

{-| Same as `blockRamFIFO` but uses Maybe to tag values to read/write -}
blockRamFIFOMaybe
    :: forall dom gated sync size a. (HiddenClockReset dom gated sync, KnownNat size, Undefined a)
    => SNat size          -- ^ FIFO size
    -> Signal dom Bool        -- ^ Read request
    -> Signal dom (Maybe a)   -- ^ Write data
    -> (
        Signal dom (Maybe a), -- Read data
        Signal dom Bool       -- Full
    )
blockRamFIFOMaybe size rReq write = (mux empty (pure Nothing) (Just <$> ramOut), full)
    where
    --Write command
    writeCommand = func <$> wEn <*> write <*> wAddr
        where 
        func False _          _     = Nothing
        func _     Nothing    _     = Nothing
        func _     (Just dat) wAddr = Just (wAddr, dat)
    --The backing ram
    ramOut = blockRam (repeat (errorX "Initial FIFO ram contents") :: Vec size a) rAddr' writeCommand
    --The status signals
    empty  = (register 0 wAddr) .==. rAddr
    full   = rAddr .==. (wrappingInc <$> wAddr)
    --Only write if there is space
    wEn    = fmap not full
    rEn    = rReq .&&. fmap not empty
    --The read and write pointers
    wAddr, rAddr :: Signal dom (Index size)
    wAddr  = register 0 $ mux (isJust <$> writeCommand) (wrappingInc <$> wAddr) wAddr
    rAddr' = mux rEn (wrappingInc <$> rAddr) rAddr
    rAddr  = register 0 rAddr'

    wrappingInc :: Index size -> Index size
    wrappingInc val
        | val == maxBound = 0
        | otherwise       = val + 1

