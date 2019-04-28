module Clash.Xilinx.Carry (
    carryN,
    carry8,
    carry4,
    xilinxCarryAdder
    ) where 

import Clash.Prelude
import Data.Bool

carryN
    :: forall n
    .  KnownNat n
    => Bool
    -> BitVector n
    -> BitVector n
    -> (BitVector n, BitVector n)
carryN cIn s d = (pack $ reverse $ zipWith xor (init c) sBits, pack $ reverse $ tail c)
    where

    sBits = reverse $ unpack s
    dBits = reverse $ unpack d

    c :: Vec (n + 1) Bool
    c = scanl carryStage cIn (zip sBits dBits)

    carryStage :: Bool -> (Bool, Bool) -> Bool
    carryStage cIn (s, d) = bool d cIn s

--TODO: primitive definitions for these
carry8 = carryN @8
carry4 = carryN @4

xilinxCarryAdder :: KnownNat n => Bool -> BitVector n -> BitVector n -> BitVector n
xilinxCarryAdder cIn x y = fst $ carryN cIn (x `xor` y) x

