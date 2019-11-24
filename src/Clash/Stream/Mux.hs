module Clash.Stream.Mux (
        streamMux
    ) where

import Clash.Prelude

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

