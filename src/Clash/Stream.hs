{-# LAnGUAGE RecordWildCards #-}
module Clash.Stream where

import Clash.Prelude

data StreamIn a = StreamIn {
    sof   :: Bool,
    eof   :: Bool,
    valid :: Bool,
    dat   :: a
} deriving (Show)

deserialize 
    :: forall dom gated sync m a. (HasClockReset dom gated sync, KnownNat m)
    => Signal dom (StreamIn a)
    -> Signal dom (Maybe (Vec m a))
deserialize streamIn = mux lastDone (Just <$> buf) (pure Nothing)
    where

    inPkt'   :: Signal dom Bool
    inPkt'   =  register False (((sof <$> streamIn) .||. inPkt') .&&. (not . eof <$> streamIn))

    inPkt    :: Signal dom Bool
    inPkt    =  (sof <$> streamIn) .||. inPkt'

    lastDone :: Signal dom Bool
    lastDone =  register False (valid <$> streamIn .&&. ptr .==. pure maxBound)

    buf      :: Signal dom (Vec m a)
    buf      =  regEn (repeat (errorX "deserialize: initial vector")) (valid <$> streamIn) $ replace <$> ptr <*> (dat <$> streamIn) <*> buf

    ptr      :: Signal dom (Index m)
    ptr      =  mux (sof <$> streamIn) (pure 0) $ register 0 $ nextPtr <$> inPkt <*> ptr <*> streamIn
        where
        nextPtr inPkt ptr StreamIn{..}
            | eof             = 0
            | not valid       = ptr
            | ptr == maxBound = 0
            | inPkt           = ptr + 1
            | otherwise       = ptr

