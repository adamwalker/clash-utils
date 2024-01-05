-- | Utilities for multiplexing streams consisting of data, and valid, ready signals
module Clash.Stream.Mux (
        streamMux,
        streamMuxLowLatency,
        streamMuxBiased
    ) where

import Clash.Prelude

import Data.Maybe (isNothing)

-- | Multiplex streams. There is a one cycle delay between when an input asserts valid and when the output valid is asserted.
streamMux
    :: forall dom m a. (HiddenClockResetEnable dom, KnownNat m, 1 <= m)
    => Signal dom Bool                    -- ^ Downstream ready
    -> Vec m (Signal dom (Bool, Bool, a)) -- ^ Incoming streams
    -> (
            Vec m (Signal dom Bool),
            Signal dom (Bool, Bool, a)
       )                                  -- ^ (Upstream readys, Outgoing streams)
streamMux ready streams = (readys, activeStream)
    where

    readys :: Vec m (Signal dom Bool)
    readys =  map (.&&. ready) $ sequenceA $ func <$> currentlyActive
        where
        func idx = map ((== idx) . Just) indicesI

    activeStream :: Signal dom (Bool, Bool, a)
    activeStream =  maybeSelect <$> sequenceA streams <*> currentlyActive
        where 
        maybeSelect vec = maybe (False, False, errorX "No stream selected") (vec !!)

    currentlyActive :: Signal dom (Maybe (Index m))
    currentlyActive = register Nothing $ func <$> currentlyActive <*> activeStream <*> ready <*> sequenceA streams
        where
        func :: Maybe (Index m) -> (Bool, Bool, a) -> Bool -> Vec m (Bool, Bool, a) -> Maybe (Index m)
        func Nothing    _               _    requests = findIndex (\(v, _, _) -> v) requests
        func (Just idx) (True, True, _) True _        = Nothing
        func st         _               _    _        = st

-- | Multiplex streams. There is no delay between when an input asserts valid and when the output valid is asserted. Makes timing a bit more difficult as a result.
streamMuxLowLatency
    :: forall dom m a. (HiddenClockResetEnable dom, KnownNat m, 1 <= m)
    => Signal dom Bool                    -- ^ Downstream ready
    -> Vec m (Signal dom (Bool, Bool, a)) -- ^ Incoming streams
    -> (
            Vec m (Signal dom Bool),
            Signal dom (Bool, Bool, a)
       )                                  -- ^ (Upstream readys, Outgoing streams)
streamMuxLowLatency ready streams = (readys, activeStream)
    where

    readys :: Vec m (Signal dom Bool)
    readys =  map (.&&. ready) $ sequenceA $ func <$> currentlyActive
        where
        func idx = map ((== idx) . Just) indicesI

    activeStream :: Signal dom (Bool, Bool, a)
    activeStream =  maybeSelect <$> sequenceA streams <*> currentlyActive
        where 
        maybeSelect vec = maybe (False, False, errorX "No stream selected") (vec !!)

    currentlyActive :: Signal dom (Maybe (Index m))
    currentlyActive = func <$> nextActive <*> sequenceA streams
        where
        func :: Maybe (Index m) -> Vec m (Bool, Bool, a) -> Maybe (Index m)
        func Nothing requests = findIndex (\(v, _, _) -> v) requests
        func st      _        = st

    nextActive :: Signal dom (Maybe (Index m))
    nextActive = register Nothing $ func <$> currentlyActive <*> activeStream <*> ready
        where
        func :: Maybe (Index m) -> (Bool, Bool, a) -> Bool -> Maybe (Index m)
        func (Just idx) (True, True, _) True = Nothing
        func st         _               _    = st

-- | Multiplex streams. There is a single cycle delay for all inputs except the first, which is combinational. 
streamMuxBiased
    :: forall dom m m' a. (HiddenClockResetEnable dom, KnownNat m, m ~ (m' + 1))
    => Signal dom Bool                    -- ^ Downstream ready
    -> Vec m (Signal dom (Bool, Bool, a)) -- ^ Incoming streams
    -> (
            Vec m (Signal dom Bool),
            Signal dom (Bool, Bool, a)
       )                                  -- ^ (Upstream readys, Outgoing streams)
streamMuxBiased ready streams = (readys, activeStream)
    where
    readys :: Vec m (Signal dom Bool)
    readys =  map (.&&. ready) $ sequenceA $ func <$> currentlyActive
        where
        func active = map func2 indicesI
            where
            func2 0   = Just 0   == active || isNothing active
            func2 idx = Just idx == active

    activeStream :: Signal dom (Bool, Bool, a)
    activeStream =  maybeSelect <$> head streams <*> sequenceA streams <*> currentlyActive
        where 
        maybeSelect def vec = maybe def (vec !!)

    hack :: Index m -> (Bool, Bool, a) -> Bool
    hack 0 (vld, eof, _) = vld && not eof
    hack _ (vld, _,   _) = vld

    currentlyActive :: Signal dom (Maybe (Index m))
    currentlyActive = register Nothing $ func <$> currentlyActive <*> activeStream <*> (imap hack <$> sequenceA streams) <*> ready
        where
        func :: Maybe (Index m) -> (Bool, Bool, a) -> Vec m Bool -> Bool -> Maybe (Index m)
        func Nothing    _               requests _    = findIndex id requests
        func (Just idx) (True, True, _) _        True = Nothing
        func st         _               _        _    = st

