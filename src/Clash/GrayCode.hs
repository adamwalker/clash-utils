{-| Conversion between binary and Gray code. https://en.wikipedia.org/wiki/Gray_code. -}
module Clash.GrayCode (
    binaryToGray,
    grayToBinary
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
    => BitVector (2 ^ n) -- ^ Gray code input
    -> BitVector (2 ^ n) -- ^ Binary output
grayToBinary x = foldl func x powers
    where 
    powers :: Vec n Int
    powers = iterate (SNat @ n) (* 2) 1
    func :: BitVector (2 ^ n) -> Int -> BitVector (2 ^ n)
    func accum power = accum `xor` (accum `shiftR` power)
