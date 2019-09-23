{-|
    Utilities for streaming samples of a signal to/from an FPGA over Ethernet.
    Useful if you want to experiment with DSP in Clash, don't have an FPGA connected to an ADC/DAC/transceiver, but do have Ethernet.

    __FPGA proven__
-}
module Clash.Stream.SampleStream (
        dropStream,
        widenStream,
        narrowStream,
        packetize,
        prependHeader,
        swapNibbles
    ) where

import Clash.Prelude
import Data.Maybe
import Clash.Container.FIFO

-- | Strip network headers from a packet
dropStream 
    :: forall dom gated sync n a
    .  (HiddenClockResetEnable dom, KnownNat n)
    => SNat n
    -> Signal dom (Bool, Bool, a)
    -> Signal dom (Bool, Bool, a)
dropStream SNat streamIn = bundle (vldOut, eofOut, datIn)
    where

    (vldIn, eofIn, datIn) = unbundle streamIn

    vldOut = (isNothing <$> cntr) .&&. vldIn
    eofOut = (isNothing <$> cntr) .&&. eofIn

    cntr :: Signal dom (Maybe (Index n))
    cntr =  regEn (Just 0) vldIn $ func <$> cntr <*> eofIn
        where
        func _           True  = Just 0
        func (Just cntr) _
            | cntr == maxBound  = Nothing
            | otherwise         = Just $ cntr + 1
        func Nothing     _      = Nothing

-- | Group data from successive clock cycles into larger chunks
widenStream 
    :: forall n dom gated sync a
    .  (HiddenClockResetEnable dom, KnownNat n, 1 <= n, NFDataX a)
    => Signal dom (Bool, Bool, a)
    -> Signal dom (Bool, Vec n a)
widenStream streamIn = bundle (vldOut, sequenceA saved)
    where

    (vldIn, eofIn, datIn) = unbundle streamIn

    --TODO: what if there are less than n then an eof?
    vldOut :: Signal dom Bool
    vldOut =  register False $ vldIn .&&. (writePtr .==. pure maxBound)

    writePtr :: Signal dom (Index n)
    writePtr =  regEn 0 vldIn $ func <$> writePtr <*> eofIn
        where
        func _        True        = 0
        func writePtr False
            | writePtr == maxBound = 0
            | otherwise            = writePtr + 1

    saved :: Vec n (Signal dom a)
    saved =  map (regEn (errorX "init") vldIn) $ saved <<+ datIn

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
    saved =  register (repeat def) $ func <$> accept <*> datIn <*> saved
        where
        func True  dat _   = dat
        func False _   vec = vec <<+ def

-- | Buffer up an entire packet worth of data and then send it out
packetize
    :: forall n dom a
    .  (HiddenClockResetEnable dom, KnownNat n, NFDataX a, Default a)
    => Signal dom (Unsigned n)
    -> Signal dom (Bool, a)
    -> Signal dom Bool
    -> Signal dom (Bool, Bool, a)
packetize pktSize streamIn downstreamReady = bundle (vldOut, eofOut, fifoDat)
    where

    (vldIn, datIn) = unbundle streamIn

    (fifoDat, _, _, nElements) = blockRamFIFO (SNat @ n) readReq datIn vldIn

    vldOut  = isJust <$> packetCnt
    eofOut  = packetCnt .==. (Just <$> pktSize)

    readReq = vldOut .&&. downstreamReady

    packetCnt :: Signal dom (Maybe (Unsigned n))
    packetCnt = register Nothing $ func <$> packetCnt <*> (nElements .>=. pktSize) <*> pktSize <*> downstreamReady
        where
        func Nothing  True  _       _     = Just 0
        func Nothing  False _       _     = Nothing
        func st        _     _      False = st
        func (Just x) _     pktSize True
            | x == pktSize = Nothing
            | otherwise    = Just $ x + 1

data PrependHeaderState n
    = Idle
    | Header (Index n)
    | Payload
    deriving (Generic, NFDataX, Eq)

-- | Prepend a header to a stream
prependHeader
    :: forall n dom a
    .  (HiddenClockResetEnable dom, Default a, KnownNat n)
    => Signal dom (Vec n a)       -- ^ Header
    -> Signal dom (Bool, Bool, a) -- ^ Stream
    -> Signal dom Bool            -- ^ Ack
    -> (Signal dom (Bool, Bool, a), Signal dom Bool)
prependHeader vec streamIn ready = (bundle (vldOut, eofOut, datOut), readyOut)
    where
    (vldIn, eofIn, datIn) = unbundle streamIn

    state :: Signal dom (PrependHeaderState n)
    state =  register Idle $ func <$> state <*> vldIn <*> ready
        where
        func Idle         True  _    = Header 0
        func (Header cnt) _     True 
            | cnt == maxBound        = Payload
            | otherwise              = Header $ cnt + 1
        func Payload      False _    = Idle
        func st           _     _    = st

    vldOut = func <$> state <*> vldIn
        where
        func Idle       _   = False
        func (Header _) _   = True
        func Payload    vld = vld

    datOut = func <$> state <*> vec <*> datIn
        where
        func Idle       _   _   = def
        func (Header i) vec _   = vec !! i
        func Payload    _   dat = dat

    readyOut = func <$> state <*> ready
        where
        func Payload ready = ready
        func _       _     = False

    eofOut = eofIn .&&. (state .==. pure Payload)

swapNibbles :: KnownNat n => Vec (2 * n) a -> Vec (2 * n) a
swapNibbles = concat . map reverse . unconcat (SNat @ 2)

