module Clash.Stream.Mux (
        streamMux,
        streamMuxBiased
    ) where

import Clash.Prelude

import Data.Maybe (isNothing)

streamMux
    :: forall dom m a. (HiddenClockResetEnable dom, KnownNat m, 1 <= m)
    => Signal dom Bool                    -- ^ Downstream ready
    -> Vec m (Signal dom (Bool, Bool, a)) -- ^ Incoming streams
    -> (
            Vec m (Signal dom Bool),      -- ^ Upstream readys
            Signal dom (Bool, Bool, a)    -- ^ Outgoing streams
    )
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
    currentlyActive = regEn Nothing ready (func <$> currentlyActive <*> activeStream <*> sequenceA streams)
        where
        func :: Maybe (Index m) -> (Bool, Bool, a) -> Vec m (Bool, Bool, a) -> Maybe (Index m)
        func Nothing    _               requests = findIndex (\(v, _, _) -> v) requests
        func (Just idx) (True, True, _) _        = Nothing
        func st         _               _        = st

streamMuxBiased
    :: forall dom m m' a. (HiddenClockResetEnable dom, KnownNat m, m ~ (m' + 1))
    => Signal dom Bool                    -- ^ Downstream ready
    -> Vec m (Signal dom (Bool, Bool, a)) -- ^ Incoming streams
    -> (
            Vec m (Signal dom Bool),      -- ^ Upstream readys
            Signal dom (Bool, Bool, a)    -- ^ Outgoing streams
    )
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
    currentlyActive = regEn Nothing ready (func <$> currentlyActive <*> activeStream <*> (imap hack <$> sequenceA streams))
        where
        func :: Maybe (Index m) -> (Bool, Bool, a) -> Vec m Bool -> Maybe (Index m)
        func Nothing    _               requests = findIndex id requests
        func (Just idx) (True, True, _) _        = Nothing
        func st         _               _        = st

