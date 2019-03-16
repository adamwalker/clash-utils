module Clash.CarryLookahead (
    koggeStone
    ) where

import Clash.Prelude

import Clash.PrefixSum

data GenProp = GenProp {
    generate  :: Bool,
    propagate :: Bool
}

instance Semigroup GenProp where
    GenProp g0 p0 <> GenProp g1 p1 = GenProp (g1 || (p1 && g0)) (p0 && p1)

koggeStone :: Bool -> BitVector 32 -> BitVector 32 -> (BitVector 32, BitVector 32)
koggeStone cIn x y = (pack $ reverse carrys, pack $ reverse sums)
    where

    genProps :: Vec 32 GenProp
    genProps =  zipWith func (reverse $ unpack x) (reverse $ unpack y)
        where
        func :: Bool -> Bool -> GenProp 
        func x y = GenProp (x && y) (x `xor` y)

    carrys :: Vec 32 Bool
    carrys = map func $ prefixSumParallel32 (<>) genProps
        where
        func (GenProp gen prop) = gen || (prop && cIn)

    sums :: Vec 32 Bool
    sums = zipWith func (cIn +>> carrys) genProps
        where
        func c (GenProp _ p) = c `xor` p

