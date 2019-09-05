{-# LANGUAGE ScopedTypeVariables #-}

{-| 
    A block ram FIFO 

    __FPGA proven__
-}
module Clash.Container.FIFO (
    blockRamFIFO,
    blockRamFIFOMaybe
    ) where

import Clash.Prelude

import Data.Maybe

{-| A FIFO backed by block ram. The input does not fall through, i.e. if the FIFO is empty and a value is written to it, that value is not available on the output in the same cycle. A current limitation of this FIFO is that its capacity is one less than the capacity of the underlying block ram. -}
blockRamFIFO 
    :: forall dom sizeBits a. (HiddenClockResetEnable dom, NFDataX a)
    => SNat sizeBits   -- ^ FIFO size
    -> Signal dom Bool -- ^ Read request
    -> Signal dom a    -- ^ Write data
    -> Signal dom Bool -- ^ Write request
    -> (
        Signal dom a,                  -- Read data
        Signal dom Bool,               -- Empty
        Signal dom Bool,               -- Full
        Signal dom (Unsigned sizeBits) -- Num elements
    )
blockRamFIFO SNat rReq wData wReq = (ramOut, empty, full, nElements)
    where
    --The backing ram
    ramOut = blockRamPow2 (repeat (errorX "Initial FIFO ram contents")) rAddr' (mux wEn (Just <$> bundle (wAddr, wData)) (pure Nothing))
    --The status signals
    empty  = (register 0 wAddr) .==. rAddr
    full   = rAddr .==. (wAddr + 1)
    --Only write if there is space
    wEn    = wReq .&&. fmap not full
    rEn    = rReq .&&. fmap not empty
    --The read and write pointers
    wAddr, rAddr :: Signal dom (Unsigned sizeBits)
    wAddr  = register 0 $ mux wEn (wAddr + 1) wAddr
    rAddr' = mux rEn (rAddr + 1) rAddr
    rAddr  = register 0 rAddr'
    --Number of elements
    nElements = wAddr - rAddr

{-| Same as `blockRamFIFO` but uses Maybe to tag values to read/write -}
blockRamFIFOMaybe
    :: forall dom sizeBits a. (HiddenClockResetEnable dom, NFDataX a)
    => SNat sizeBits          -- ^ FIFO size
    -> Signal dom Bool        -- ^ Read request
    -> Signal dom (Maybe a)   -- ^ Write data
    -> (
        Signal dom (Maybe a),          -- Read data
        Signal dom Bool,               -- Full
        Signal dom (Unsigned sizeBits) -- Num elements
    )
blockRamFIFOMaybe SNat rReq write = (mux empty (pure Nothing) (Just <$> ramOut), full, nElements)
    where
    --Write command
    writeCommand = func <$> wEn <*> write <*> wAddr
        where 
        func False _          _     = Nothing
        func _     Nothing    _     = Nothing
        func _     (Just dat) wAddr = Just (wAddr, dat)
    --The backing ram
    ramOut = blockRamPow2 (repeat (errorX "Initial FIFO ram contents")) rAddr' writeCommand
    --The status signals
    empty  = (register 0 wAddr) .==. rAddr
    full   = rAddr .==. (wAddr + 1)
    --Only write if there is space
    wEn    = fmap not full
    rEn    = rReq .&&. fmap not empty
    --The read and write pointers
    wAddr, rAddr :: Signal dom (Unsigned sizeBits)
    wAddr  = register 0 $ mux (isJust <$> writeCommand) (wAddr + 1) wAddr
    rAddr' = mux rEn (rAddr + 1) rAddr
    rAddr  = register 0 rAddr'
    --Number of elements
    nElements = wAddr - rAddr

