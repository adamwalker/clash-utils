module CLaSH.GrayCode (
    binaryToGray,
    grayToBinary32
    ) where

import CLaSH.Prelude

binaryToGray 
    :: KnownNat n 
    => BitVector n 
    -> BitVector n
binaryToGray x = x `xor` (x `shiftR` 1)

grayToBinary32
    :: BitVector 32
    -> BitVector 32
grayToBinary32 x5 = x0
    where 
    x0 = x1 `xor` (x1 `shiftR` 1)
    x1 = x2 `xor` (x2 `shiftR` 2)
    x2 = x3 `xor` (x3 `shiftR` 4)
    x3 = x4 `xor` (x4 `shiftR` 8)
    x4 = x5 `xor` (x5 `shiftR` 16)
