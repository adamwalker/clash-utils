module Clash.DSP.FIR.Polyphase (
        Filter,
        polyphaseDecim
    ) where

import Clash.Prelude
import Data.Maybe

import Clash.Counter
import Clash.DSP.FIR.SemiParallel(integrateAndDump)

type Filter dom a
    =  Signal dom Bool                                  -- ^ Input valid
    -> Signal dom a                                     -- ^ Sample
    -> (Signal dom Bool, Signal dom a, Signal dom Bool) -- ^ (Output valid, output data, ready)

polyphaseDecim
    :: forall numPhases numPhases0 a dom
    .  (HiddenClockResetEnable dom, KnownNat numPhases, 1 <= numPhases, numPhases ~ (numPhases0 + 1), Num a, NFDataX a)
    => Vec numPhases (Filter dom a)                     -- ^ Sub filtes for each phase
    -> Signal dom Bool                                  -- ^ Input valid
    -> Signal dom a                                     -- ^ Data In
    -> (Signal dom Bool, Signal dom a, Signal dom Bool) -- ^ (Output valid, output data, ready)
polyphaseDecim filters valid sampleIn = (outputValid, summed, inputReady)
    where

    --Track which filter phase will be receiving the next input
    activePhase :: Signal dom (Index numPhases)
    activePhase = wrappingCounter 0 (inputReady .&&. valid)

    --We can accept an input if the current phase is ready
    inputReady :: Signal dom Bool
    inputReady =  liftA2 (!!) (sequenceA readys) activePhase

    --Instantiate the sub-filters
    valids :: Vec numPhases (Signal dom Bool)
    dats   :: Vec numPhases (Signal dom a)
    readys :: Vec numPhases (Signal dom Bool)
    (valids, dats, readys) = unzip3 $ imap func filters
        where
        func idx filter = filter (valid .&&. activePhase .==. pure idx) sampleIn 

    --Track which filter phase output is expected next
    activeOutputPhase :: Signal dom (Index numPhases)
    activeOutputPhase = wrappingCounter 0 currentIsValid

    --Wait until the next phase is valid
    currentIsValid :: Signal dom Bool
    currentIsValid =  liftA2 (!!) (sequenceA valids) activeOutputPhase

    --Sum the filter outputs
    summed = integrateAndDump 
        currentIsValid 
        (activeOutputPhase .==. 0) 
        0
        (liftA2 (!!) (sequenceA dats) activeOutputPhase)

    outputValid = register False $ last valids .&&. (activeOutputPhase .==. (pure maxBound))

