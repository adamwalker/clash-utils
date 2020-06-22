{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}

{-| A hardware divider -}
module Clash.Arithmetic.Divide (
    divideStep,
    combDivide
    ) where

import Clash.Prelude

{-| Perform one step of the restoring division algorithm -}
divideStep 
    :: KnownNat n  
    => Vec n Bit        -- ^ Divisor
    -> Vec n Bit        -- ^ Divider state
    -> Bit              -- ^ Next msb
    -> (Vec n Bit, Bit) -- ^ (Output divider state, next bit of output)
divideStep divisor p b
    | divisor > shiftedP = (shiftedP,                0)
    | otherwise          = (vecSub shiftedP divisor, 1)
    where
    shiftedP = p <<+ b
    vecSub :: (KnownNat m) => Vec m Bit -> Vec m Bit -> Vec m Bit
    vecSub x y = unpack $ pack x - pack y

{-| Perform division combinationally. To achieve a high clock frequency with any reasonable sized division, you will probably want to use divideStep and split up the computation into multiple clock cycles. -}
combDivide 
    :: forall n. KnownNat n 
    => BitVector n                -- ^ Number to be divided
    -> BitVector n                -- ^ Divisor
    -> (BitVector n, BitVector n) -- ^ (quotient, remainder)
combDivide x y = (pack q, pack r)
    where
    (r, q) = mapAccumL (divideStep (unpack y)) (repeat 0) (unpack x)
