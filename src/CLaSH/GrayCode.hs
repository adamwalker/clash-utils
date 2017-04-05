module CLaSH.GrayCode (
    binaryToGray,
    grayToBinary
    ) where

import CLaSH.Prelude

binaryToGray 
    :: KnownNat n 
    => BitVector n 
    -> BitVector n
binaryToGray x = x `xor` (x `shiftR` 1)

grayToBinary
    :: forall n. KnownNat n
    => BitVector (2 ^ n)
    -> BitVector (2 ^ n)
grayToBinary x = foldl func x powers
    where 
    powers :: Vec n Int
    powers = iterate (SNat @ n) (* 2) 1
    func :: BitVector (2 ^ n) -> Int -> BitVector (2 ^ n)
    func accum power = accum `xor` (accum `shiftR` power)
