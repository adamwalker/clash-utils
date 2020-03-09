{-|
    __FPGA proven__
-}
module Clash.Stream.Packet (
        dropStream,
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
    packetCnt = register Nothing $ func <$> packetCnt <*> (nElements .>. pktSize) <*> pktSize <*> downstreamReady
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
    state =  register Idle $ func <$> state <*> vldIn <*> ready <*> eofIn
        where
        func Idle         True  _    _    = Header 0
        func (Header cnt) _     True _
            | cnt == maxBound             = Payload
            | otherwise                   = Header $ cnt + 1
        func Payload      True  _    True = Idle
        func st           _     _    _    = st

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

