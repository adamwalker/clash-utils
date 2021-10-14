module Clash.Arithmetic.Adder.CarrySave (
        fullAdder,
        compressor,
        compressorN
    ) where

import Clash.Prelude
import Data.Tuple

fullAdder :: Bool -> Bool -> Bool -> (Bool, Bool)
fullAdder cIn x y = unpack $ (resize (pack cIn) :: BitVector 2) + resize (pack x) + resize (pack y)

compressor 
    :: forall carrys
    .  Vec carrys Bool
    -> Vec (carrys + 3) Bool
    -> (Vec carrys Bool, Vec 2 Bool)
compressor cIns toSum = (carrys, msb :> lsb :> Nil)
    where
    s1 :> s2 :> s3 :> sRest = toSum

    (lsb, carrys :< msb) = mapAccumL func s1 $ (s2, s3) :> zip cIns sRest
        where
        func :: Bool -> (Bool, Bool) -> (Bool, Bool)
        func x (c, y) = swap $ fullAdder c x y

compressorN
    :: forall carrys n
    .  (KnownNat n)
    => Vec carrys Bool
    -> Vec (carrys + 3) (BitVector n)
    -> (Vec carrys Bool, Vec 2 (BitVector n))
compressorN cIns toSum = (cOuts, pack (reverse resMsb) :> pack (reverse resLsb) :> Nil)
    where
    toSumVec :: Vec n (Vec (carrys + 3) Bool)
    toSumVec =  transpose $ map (reverse . unpack) toSum

    (cOuts, sumVecs) = mapAccumL compressor cIns toSumVec

    (resMsb :> resLsb :> Nil) = transpose sumVecs

