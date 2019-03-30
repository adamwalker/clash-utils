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
    -> Vec n Bool
    -> Vec n Bool
    -> (Vec n Bool, Vec n Bool)
carryN cIn s d = (zipWith xor (init c) s, tail c)
    where

    c :: Vec (n + 1) Bool
    c = scanl carryStage cIn (zip s d)

    carryStage :: Bool -> (Bool, Bool) -> Bool
    carryStage cIn (s, d) = bool d cIn s

--TODO: primitive definitions for these
carry8 = carryN @8
carry4 = carryN @4

xilinxCarryAdder :: KnownNat n => Bool -> BitVector n -> BitVector n -> BitVector n
xilinxCarryAdder cIn x y = pack $ reverse $ fst $ 
    carryN cIn 
        (reverse $ unpack $ x `xor` y) 
        (reverse $ unpack x)

