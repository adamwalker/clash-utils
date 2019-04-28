module Clash.CarrySelect (
    CarryN,
    carrySelectLeaf,
    carrySelectStage,
    carryNAdder,
    carrySelectAdderExample
    ) where

import Clash.Prelude
import Data.Bool

type CarryN n
    =  Bool
    -> BitVector n
    -> BitVector n
    -> (BitVector n, BitVector n)

carrySelectLeaf
    :: forall chunk n chunk'
    .  (KnownNat n, KnownNat chunk, chunk ~ (chunk' + 1))
    => CarryN chunk
    -> BitVector (n * chunk)
    -> BitVector (n * chunk)
    -> Vec n ((Bool, Bool), (BitVector chunk, BitVector chunk))
carrySelectLeaf carryN x y = zipWith func (unpack x) (unpack y)
    where
    func :: BitVector chunk -> BitVector chunk -> ((Bool, Bool), (BitVector chunk, BitVector chunk))
    func x y = ((bitToBool $ msb cs0, bitToBool $ msb cs1), (s0, s1))
        where
        gen  = y
        prop = x `xor` y

        (s0, cs0) = carryN False prop gen
        (s1, cs1) = carryN True  prop gen

carrySelectStage 
    :: forall level chunk c
    .  (chunk ~ (c + 1), KnownNat chunk, KnownNat level)
    => CarryN chunk
    -> Vec chunk ((Bool, Bool), (BitVector (chunk ^ level), BitVector (chunk ^ level)))
    -> ((Bool, Bool), (BitVector (chunk ^ (level + 1)), BitVector (chunk ^ (level + 1))))
carrySelectStage carryN inputs = (head prefixSum, (pack sum0, pack sum1))

    where
    prefixSum :: Vec chunk (Bool, Bool)
    prefixSum = zip (unpack c0) (unpack c1)
        where
        (s, d) = unzip $ map (func . fst) inputs
            where
            func (gen, prop) = (prop && not gen, gen)

        (_, c0) = carryN False (pack s) (pack d)
        (_, c1) = carryN True  (pack s) (pack d)

    --Sum possibilities assuming the value of carry in
    sum0s, sum1s :: Vec chunk (BitVector (chunk ^ level))
    sum0s = map (fst . snd) inputs
    sum1s = map (snd . snd) inputs

    sum0, sum1 :: Vec chunk (BitVector (chunk ^ level))
    sum0 = zipWith3 bool sum0s sum1s $ map fst (tail prefixSum) :< False
    sum1 = zipWith3 bool sum0s sum1s $ map snd (tail prefixSum) :< True

carryNAdder :: forall n. KnownNat n => CarryN n
carryNAdder cIn s d = (truncateB sum, pack $ takeI $ (unpack $ sum `xor` resize x0 `xor` resize x1 :: Vec (n + 1) Bool))
    where

    x0 = s .|. d
    x1 = complement s .&. d

    sum :: BitVector (n + 1)
    sum = extend x1 + extend x0 + extend (pack cIn)

carrySelectAdderExample :: BitVector 512 -> BitVector 512 -> BitVector 512
carrySelectAdderExample x y = fst $ snd $ carrySelectStage @2 carryNAdder $ map (carrySelectStage @1 carryNAdder) leaves
    where
    leaves :: Vec 8 (Vec 8 ((Bool, Bool), (BitVector 8, BitVector 8)))
    leaves = unconcatI $ carrySelectLeaf @8 @64 carryNAdder x y

