{-|
    __FPGA proven__
-}
module Clash.Stream.Resize (
        widenStream,
        narrowStream
    ) where

import Clash.Prelude
import Data.Maybe
import Clash.Container.FIFO

-- | Group data from successive clock cycles into larger chunks
widenStream
    :: forall n dom gated sync a
    .  (HiddenClockResetEnable dom, KnownNat n, 1 <= n, NFDataX a)
    => Signal dom (Bool, Bool, a)
    -> Signal dom Bool
    -> (Signal dom (Bool, Bool, Vec n a), Signal dom Bool)
widenStream streamIn readyOut = (bundle (vldOut, eofOut, sequenceA saved), step)
    where

    step = fmap not vldOut .||. readyOut

    (vldIn, eofIn, datIn) = unbundle streamIn

    --TODO: what if there are less than n then an eof?
    vldOut :: Signal dom Bool
    vldOut =  regEn False step $ vldIn .&&. (writePtr .==. pure maxBound)

    eofOut :: Signal dom Bool
    eofOut =  regEn False step eofIn

    writePtr :: Signal dom (Index n)
    writePtr =  regEn 0 (vldIn .&&. step) $ func <$> writePtr <*> eofIn
        where
        func _        True        = 0
        func writePtr False
            | writePtr == maxBound = 0
            | otherwise            = writePtr + 1

    saved :: Vec n (Signal dom a)
    saved =  sequenceA $ regEn (repeat (errorX "init")) (vldIn .&&. step) $ sequenceA $ saved <<+ datIn

-- | Break up a stream of data into a stream of smaller chunks
narrowStream 
    :: forall n dom gated sync a
    .  (HiddenClockResetEnable dom, NFDataX a, Default a, KnownNat n)
    => Signal dom (Bool, Bool, Vec (n + 1) a)
    -> Signal dom Bool
    -> (Signal dom (Bool, Bool, a), Signal dom Bool)
narrowStream streamIn readyIn = (bundle (vldOut, eofOut, head <$> saved), readyOut)
    where

    (vldIn, eofIn, datIn) = unbundle streamIn

    vldOut :: Signal dom Bool
    vldOut =  isJust <$> readPtr

    accept :: Signal dom Bool
    accept = vldIn .&&. readyOut

    latchedEof :: Signal dom Bool
    latchedEof =  regEn False accept eofIn

    eofOut :: Signal dom Bool
    eofOut =  latchedEof .&&. (readPtr .==. (Just <$> pure maxBound))

    readyOut :: Signal dom Bool
    readyOut 
        =    (isNothing <$> readPtr) 
        .||. ((readPtr .==. (Just <$> pure maxBound)) .&&. readyIn)

    readPtr :: Signal dom (Maybe (Index (n + 1)))
    readPtr =  register Nothing $ func <$> readPtr <*> vldIn <*> readyIn
        where
        func Nothing True  _ = Just 0
        func Nothing False _ = Nothing
        func (Just readPtr) vldIn True
            | readPtr == maxBound && vldIn = Just 0
            | readPtr == maxBound          = Nothing
            | otherwise                    = Just $ readPtr + 1
        func st _ False = st

    saved :: Signal dom (Vec (n + 1) a)
    saved =  register (repeat def) $ func <$> accept <*> datIn <*> saved <*> readyIn
        where
        func True  dat _   _    = dat
        func False _   vec True = vec <<+ def
        func _     _   vec _    = vec

