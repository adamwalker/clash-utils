module Clash.CarryLookahead (
    carryLookaheadAdder,
    koggeStone,
    brentKung
    ) where

import Clash.Prelude

import Clash.PrefixSum

data GenProp = GenProp {
    generate  :: Bool,
    propagate :: Bool
}

instance Semigroup GenProp where
    GenProp g0 p0 <> GenProp g1 p1 = GenProp (g1 || (p1 && g0)) (p0 && p1)

type PrefixSum n a = (a -> a -> a) -> Vec n a -> Vec n a

carryLookaheadAdder :: forall n. KnownNat n => PrefixSum n GenProp -> Bool -> BitVector n -> BitVector n -> (BitVector n, BitVector n)
carryLookaheadAdder prefixSum cIn x y = (pack $ reverse carrys, pack $ reverse sums)
    where

    genProps :: Vec n GenProp
    genProps =  zipWith func (reverse $ unpack x) (reverse $ unpack y)
        where
        func :: Bool -> Bool -> GenProp 
        func x y = GenProp (x && y) (x `xor` y)

    carrys :: Vec n Bool
    carrys = map func $ prefixSum (<>) genProps
        where
        func (GenProp gen prop) = gen || (prop && cIn)

    sums :: Vec n Bool
    sums = zipWith func (cIn +>> carrys) genProps
        where
        func c (GenProp _ p) = c `xor` p

koggeStone = carryLookaheadAdder prefixSumParallel32
brentKung  = carryLookaheadAdder prefixSumWorkEfficient32

