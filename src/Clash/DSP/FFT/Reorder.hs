module Clash.DSP.FFT.Reorder (
    bitReversalReorder
    ) where

import Clash.Prelude
import Clash.Counter
import Clash.Misc

bitReversalReorder
    :: forall dom n a
    .  HiddenClockResetEnable dom
    => NFDataX a
    => SNat n
    -> Signal dom Bool
    -> Signal dom a
    -> Signal dom a
bitReversalReorder SNat en dat = ramOut
    where

    counter :: Signal dom (BitVector (n + 1))
    counter =  count 0 en

    (stage :: Signal dom Bool, address' :: Signal dom (BitVector n)) 
        = unbundle $ bitCoerce <$> counter

    address :: Signal dom (Unsigned n)
    address = unpack <$> address'

    addressReversed :: Signal dom (Unsigned n)
    addressReversed = unpack . revBV <$> address'

    addressFinal = mux stage address addressReversed

    ramOut = blockRamPow2 (repeat (errorX "initial reorder ram") :: Vec (2 ^ n) a) addressFinal
        $ mux en (Just <$> bundle (addressFinal, dat)) (pure Nothing)

