{-# LANGUAGE ScopedTypeVariables #-}

{-| A block ram FIFO -}
module CLaSH.FIFO (
    blockRamFIFO
    ) where

import CLaSH.Prelude

{-| A FIFO backed by block ram. The input does not fall through, i.e. if the FIFO is empty and a value is written to it, that value is not available on the output in the same cycle. A current limitation of this FIFO is that its capacity is one less than the capacity of the underlying block ram. -}
blockRamFIFO 
    :: forall size a. (KnownNat size, Default a)
    => SNat size   -- ^ FIFO size
    -> Signal Bool -- ^ Read request
    -> Signal a    -- ^ Write data
    -> Signal Bool -- ^ Write request
    -> (
        Signal a,    -- Read data
        Signal Bool, -- Empty
        Signal Bool  -- Full
    )
blockRamFIFO size rReq wData wReq = (ramOut, empty, full)
    where
    --The backing ram
    ramOut = readNew (blockRam (repeat def :: Vec size a)) rAddr' (mux wEn (Just <$> bundle (wAddr, wData)) (pure Nothing))
    --The status signals
    empty  = wAddr .==. rAddr
    full   = rAddr .==. (wrappingInc <$> wAddr)
    --Only write if there is space
    wEn    = wReq .&&. fmap not full
    --The read and write pointers
    wAddr, rAddr :: Signal (Index size)
    wAddr = register 0 $ mux wEn            (wrappingInc <$> wAddr) wAddr
    rAddr' = mux (rReq .&&. fmap not empty) (wrappingInc <$> rAddr) rAddr
    rAddr = register 0 rAddr'

    wrappingInc :: Index size -> Index size
    wrappingInc val
        | val == maxBound = 0
        | otherwise       = val + 1

