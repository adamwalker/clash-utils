{-# LANGUAGE ScopedTypeVariables #-}

{-| Binary to BCD conversion using the <https://en.wikipedia.org/wiki/Double_dabble Double Dabble> algorithm. -}
module Clash.BCD (
    BCDDigit,
    convertStep,
    convertSteps,
    toDec,
    bcdToAscii,
    asciiToBCD,
    asciisToBCDs,
    bcdSub,
    bcdAdd
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

bcdToAscii :: BitVector 4 -> BitVector 8
bcdToAscii x = 0x3 ++# x

asciiToBCD :: BitVector 8 -> Maybe (BitVector 4)
asciiToBCD digit 
    |  highNibble == 0x3 
    && lowNibble < 10    = Just lowNibble
    |  otherwise         = Nothing
    where
    (highNibble :: BitVector 4, lowNibble :: BitVector 4) = split digit

asciisToBCDs :: (KnownNat n, 1 <= n) =>  Vec n (BitVector 8) -> Maybe (Vec n (BitVector 4))
asciisToBCDs = sequenceA . map asciiToBCD

bcdSub 
    :: forall n. (KnownNat n) 
    => Vec n (BitVector 4) 
    -> Vec n (BitVector 4) 
    -> (BitVector 1, Vec n (BitVector 4))
bcdSub x y = (bitCoerce cOut, zipWith resolveCarry carryBits sumRes)
    where

    resolveCarry :: Bool -> BitVector 4 -> BitVector 4
    resolveCarry False res = res - 6
    resolveCarry True  res = res

    sumRes :: Vec n (BitVector 4)
    cOut   :: Bool
    (cOut, sumRes) = unpack $ resize (pack x) + resize (complement (pack y)) + 1

    --TODO: directly instantiate a carry chain primitive to get rid of this nonsense
    carryBits :: Vec n Bool
    carryBits = init $ cOut :> zipWith3 func sumRes x y
        where
        func :: BitVector 4 -> BitVector 4 -> BitVector 4 -> Bool
        func r x y = bitCoerce $ lsb r `xor` lsb x `xor` complement (lsb y)

bcdAdd 
    :: forall n. (KnownNat n) 
    => Vec n (BitVector 4) 
    -> Vec n (BitVector 4) 
    -> (BitVector 1, Vec n (BitVector 4))
bcdAdd x y = (bitCoerce cOut, zipWith resolveCarry carryBits sumRes)
    where

    resolveCarry :: Bool -> BitVector 4 -> BitVector 4
    resolveCarry False res = res - 6
    resolveCarry True  res = res

    sumRes :: Vec n (BitVector 4)
    cOut   :: Bool
    (cOut, sumRes) = unpack $ resize (pack x) + resize (pack (map (+6) y))

    --TODO: directly instantiate a carry chain primitive to get rid of this nonsense
    carryBits :: Vec n Bool
    carryBits = init $ cOut :> zipWith3 func sumRes x y
        where
        func :: BitVector 4 -> BitVector 4 -> BitVector 4 -> Bool
        func r x y = bitCoerce $ lsb r `xor` lsb x `xor` lsb y

