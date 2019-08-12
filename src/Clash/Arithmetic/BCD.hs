{-# LANGUAGE ScopedTypeVariables #-}

{-| 
    BCD arithmetic and binary to BCD conversion using the <https://en.wikipedia.org/wiki/Double_dabble Double Dabble> algorithm. 

    __FPGA proven__
-}
module Clash.Arithmetic.BCD (
    BCDDigit,
    convertStep,
    convertSteps,
    toDec,
    binaryToDecExample,
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

{-| An example synthesizeable binary to decimal conversion. Converts an 8 bit binary number to three BCD digits. Consists of a 2 stage pipeline. Each pipeline stage performs four double dabble iterations for a total of 8 iterations. Processes one input per cycle. Latency is 2 cycles. -}
binaryToDecExample 
    :: HiddenClockResetEnable dom 
    => Signal dom (BitVector 8)  -- ^ Input binary number
    -> Signal dom (BitVector 12) -- ^ Output BCD number
binaryToDecExample x 
    = fmap pack 
    $ liftA2 convertSteps (slice d3 d0 <$> x) 
    $ register (repeat 0) 
    $ liftA2 convertSteps (slice d7 d4 <$> x)
    $ pure (repeat 0)

{-| Convert a BCD digit to ASCII -}
bcdToAscii 
    :: BitVector 4 -- ^ Input BCD digit
    -> BitVector 8 -- ^ Output ASCII symbol
bcdToAscii x = 0x3 ++# x

{-| Convert an ASCII symbol to BCD -}
asciiToBCD 
    :: BitVector 8         -- ^ Input ASCII symbol
    -> Maybe (BitVector 4) -- ^ Output BCD digit (if the conversion succeeds)
asciiToBCD digit 
    |  highNibble == 0x3 
    && lowNibble < 10    = Just lowNibble
    |  otherwise         = Nothing
    where
    (highNibble :: BitVector 4, lowNibble :: BitVector 4) = split digit

{-| Convert an ASCII string to BCD digits -}
asciisToBCDs 
    :: (KnownNat n, 1 <= n) 
    =>  Vec n (BitVector 8)        -- ^ Input ASCII string
    -> Maybe (Vec n (BitVector 4)) -- ^ Output BCD digits (if the conversion succeeds)
asciisToBCDs = sequenceA . map asciiToBCD

{-| BCD subtraction. Efficiently utilises the carry chain. -}
bcdSub 
    :: forall n. (KnownNat n) 
    => Vec n (BitVector 4)                -- ^ First input
    -> Vec n (BitVector 4)                -- ^ Second input
    -> (BitVector 1, Vec n (BitVector 4)) -- ^ (carry out, result)
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

{-| BCD addition. Efficiently utilises the carry chain. -}
bcdAdd 
    :: forall n. (KnownNat n) 
    => Vec n (BitVector 4)                -- ^ First input
    -> Vec n (BitVector 4)                -- ^ Second input
    -> (BitVector 1, Vec n (BitVector 4)) -- ^ (carry out, result)
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

