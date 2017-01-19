{-# LANGUAGE ScopedTypeVariables #-}

{-| A block ram FIFO -}
module CLaSH.FIFO (
    blockRamFIFO
    ) where

import CLaSH.Prelude

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
    ramOut = blockRam (repeat def :: Vec size a) rAddr (mux wEn (Just <$> bundle (wAddr, wData)) (pure Nothing))
    --The status signals
    empty  = wAddr .==. rAddr
    full   = rAddr .==. (wrappingInc <$> wAddr)
    --Only write if there is space
    wEn    = wReq .&&. fmap not full
    --The read and write pointers
    wAddr, rAddr :: Signal (Index size)
    wAddr = register 0 $ mux wEn                    (wrappingInc <$> wAddr) wAddr
    rAddr = register 0 $ mux (rReq .&&. fmap not empty) (wrappingInc <$> rAddr) rAddr

    wrappingInc :: Index size -> Index size
    wrappingInc val
        | val == maxBound = 0
        | otherwise       = val + 1
