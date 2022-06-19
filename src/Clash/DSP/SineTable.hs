{-| https://zipcpu.com/dsp/2017/08/26/quarterwave.html

    __FPGA proven__
 -}
module Clash.DSP.SineTable (
        sines,
        sineTable',
        sineTable,
        cosineTable
    ) where

import Clash.Prelude
import qualified Prelude as P

{-| Compute the values for the sine lookup table -}
sines :: Int -> [Double]
sines size 
    = P.take size 
    $ P.map (\x -> sin(2*pi*(2*(fromIntegral x)+1)/(8*fromIntegral size)))
    $ [0..size-1]

sineTable'
    :: forall dom n m a
    .  HiddenClockResetEnable dom
    => KnownNat n
    => KnownNat m
    => Vec (2 ^ n) (UFixed 0 m)
    -> Signal dom (Unsigned 2)
    -> Signal dom (Unsigned n)
    -> Signal dom (SFixed 1 m)
sineTable' table quadrant addr = mux negD (negate <$> signed) signed
    where

    (neg :: Signal dom Bool, flip :: Signal dom Bool)
        = unbundle $ bitCoerce <$> quadrant

    --Save the negation signal for the cycle after the ram read
    negD = delay (errorX "inital negD") neg

    --The table ram
    tableRes = blockRam table (mux flip (complement <$> addr) addr) (pure Nothing)

    --Make it signed
    signed :: Signal dom (SFixed 1 m)
    signed =  bitCoerce . (False,) <$> tableRes

sineTable
    :: forall dom n m a
    .  HiddenClockResetEnable dom
    => KnownNat n
    => KnownNat m
    => Vec (2 ^ n) (UFixed 0 m)
    -> Signal dom (Unsigned (n + 2))
    -> Signal dom (SFixed 1 m)
sineTable table addr = sineTable' table quadrant addr'
    where
    (quadrant, addr')
        = unbundle $ bitCoerce <$> addr

cosineTable
    :: forall dom n m a
    .  HiddenClockResetEnable dom
    => KnownNat n
    => KnownNat m
    => Vec (2 ^ n) (UFixed 0 m)
    -> Signal dom (Unsigned (n + 2))
    -> Signal dom (SFixed 1 m)
cosineTable table addr = sineTable' table (quadrant + 1) addr'
    where
    (quadrant, addr')
        = unbundle $ bitCoerce <$> addr

