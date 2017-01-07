{-# LANGUAGE ScopedTypeVariables #-}

{-| Binary to BCD conversion using the <https://en.wikipedia.org/wiki/Double_dabble Double Dabble> algorithm. -}
module CLaSH.BCD (
    BCDDigit,
    convertStep,
    singleCycleConvert,
    toDec
    ) where

import CLaSH.Prelude

type BCDDigit = Unsigned 4

add3 :: BCDDigit -> BCDDigit
add3 digit
    | digit > 4 = digit + 3
    | otherwise = digit

convertStep 
    :: forall n. (KnownNat (n * 4), KnownNat n) 
    => Bit 
    -> Vec n BCDDigit 
    -> Vec n BCDDigit
convertStep bit digits = unpack $ pack shifted
    where
    --dabble
    dabbled   =  map add3 digits
    --Flatten the BCD to a vector of bits
    flattened :: Vec (n * 4) Bit
    flattened = unpack $ pack dabbled
    --shift in the input bit
    shifted   = flattened <<+ bit

singleCycleConvert 
    :: (KnownNat n, KnownNat m, KnownNat (m * 4))
    => BitVector n
    -> Vec m BCDDigit
    -> Vec m BCDDigit
singleCycleConvert n d = foldl (flip convertStep) d (unpack n)

toDec :: (KnownNat n, KnownNat m, KnownNat (m * 4)) => BitVector n -> Vec m BCDDigit
toDec = flip singleCycleConvert (repeat 0)
