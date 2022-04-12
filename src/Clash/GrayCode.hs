{-| Conversion between binary and Gray code. https://en.wikipedia.org/wiki/Gray_code. -}
module Clash.GrayCode (
    binaryToGray,
    grayToBinary,
    grayToBinaryLogDepth
    ) where

import Clash.Prelude

{-| Binary to Gray code conversion -}
binaryToGray 
    :: KnownNat n 
    => BitVector n -- ^ Binary input
    -> BitVector n -- ^ Gray code output
binaryToGray x = x `xor` (x `shiftR` 1)

{-| Gray code to binary conversion -}
grayToBinary
    :: forall n. KnownNat n
    => BitVector n -- ^ Gray code input
    -> BitVector n -- ^ Binary output
grayToBinary = pack . postscanl xor False . unpack

{-| Gray code to binary conversion -}
grayToBinaryLogDepth
    :: forall n. KnownNat n
    => BitVector (2 ^ n) -- ^ Gray code input
    -> BitVector (2 ^ n) -- ^ Binary output
grayToBinaryLogDepth x = foldl func x powers
    where 
    powers :: Vec n Int
    powers = iterate (SNat @n) (* 2) 1
    func :: BitVector (2 ^ n) -> Int -> BitVector (2 ^ n)
    func accum power = accum `xor` (accum `shiftR` power)

