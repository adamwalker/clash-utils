{-# LANGUAGE ScopedTypeVariables #-}

{-| Binary to BCD conversion using the <https://en.wikipedia.org/wiki/Double_dabble Double Dabble> algorithm. -}
module Clash.BCD (
    BCDDigit,
    convertStep,
    convertSteps,
    toDec
    ) where

import Clash.Prelude

type BCDDigit = Unsigned 4

add3 :: BCDDigit -> BCDDigit
add3 digit
    | digit > 4 = digit + 3
    | otherwise = digit

{-| Perform one iteration of the double dabble algorithm - shifting one bit in from the right -}
convertStep 
    :: forall n. KnownNat n
    => Bit            -- ^ Bit to shift in
    -> Vec n BCDDigit -- ^ BCD digit scratch space (as the Wikipedia page calls it)
    -> Vec n BCDDigit -- ^ Updated BCD scratch space
convertStep bit digits = unpack $ pack shifted
    where
    --dabble
    dabbled   =  map add3 digits
    --Flatten the BCD to a vector of bits
    flattened :: Vec (n * 4) Bit
    flattened = unpack $ pack dabbled
    --shift in the input bit
    shifted   = flattened <<+ bit

{-| Perform several iterations of the double dabble algorithm - shifting in several bits from the right -}
convertSteps 
    :: (KnownNat n, KnownNat m)
    => BitVector n    -- ^ Bits to shift in
    -> Vec m BCDDigit -- ^ BCD digit scratch space (as the Wikipedia page calls it)
    -> Vec m BCDDigit -- ^ Updated BCD scratch space
convertSteps n d = foldl (flip convertStep) d (unpack n)

{-| Combinationally convert a binary number to BCD. Use `convertSteps` instead if you need to split up the calculation over several clock cycles. -}
toDec :: (KnownNat n, KnownNat m) 
    => BitVector n    -- ^ Binary number to convert
    -> Vec m BCDDigit -- ^ Vector of BCD digits
toDec = flip convertSteps (repeat 0)

