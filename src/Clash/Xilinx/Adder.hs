module Clash.Xilinx.Adder (
        compressor,
        ternaryAdder
    ) where

import Clash.Prelude

import Clash.Arithmetic.Adder.CarrySave hiding (compressor)
import Clash.Xilinx.Carry

compressor
    :: forall n m a
    .  (KnownNat n, n ~ (m + 1))
    => Bool
    -> Vec 4 (Vec n Bool)
    -> (Vec (n + 1) Bool, Vec n Bool)
compressor cIn (w :> x :> y :> z :> Nil) = (cOut :> unpack sums, cs)
    where
    cs, ss :: Vec n Bool
    (cs, ss) = unzip $ zipWith3 fullAdder w x y

    selects :: Vec n Bool
    selects =  zipWith xor ss z

    (sums, carrys') = carryN cIn (pack selects) (pack z)

    cOut :: Bool
    cOut :> _ = unpack carrys'

ternaryAdder
    :: forall n m a
    .  (KnownNat n, n ~ (m + 1))
    => Bool
    -> Bool
    -> Vec 3 (BitVector n)
    -> (BitVector (n + 1), Bool)
ternaryAdder cIn cIn2 xs = (pack sums, cOut)
    where
    (sums, cOut :> carrys) = compressor cIn (map unpack xs :< (carrys :< cIn2))

