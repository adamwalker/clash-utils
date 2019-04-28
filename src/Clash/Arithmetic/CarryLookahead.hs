{-| Carry lookahead adders built on prefix sums: https://en.wikipedia.org/wiki/Carry-lookahead_adder 

    Don't use these for adders on FPGAs. They already come with fast carry chains in hard silicon.

    __FPGA proven__
-}
module Clash.Arithmetic.CarryLookahead (
    PrefixSum,
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

-- | A generic carry lookahead parameterised by the type of prefix sum it uses
carryLookaheadAdder 
    :: forall n. KnownNat n 
    => PrefixSum n GenProp        -- ^ The prefix sum structure to use
    -> Bool                       -- ^ Carry in
    -> BitVector n                -- ^ Sum input 1
    -> BitVector n                -- ^ Sum input 2
    -> (BitVector n, BitVector n) -- ^ (Intermediate carrys, sum outputs)
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

{-| A [Kogge-Stone](https://en.wikipedia.org/wiki/Kogge%E2%80%93Stone_adder) adder -}
koggeStone = carryLookaheadAdder prefixSumParallel32

{-| A [Brent-Kung](https://en.wikipedia.org/wiki/Brent%E2%80%93Kung_adder) adder -}
brentKung  = carryLookaheadAdder prefixSumWorkEfficient32

