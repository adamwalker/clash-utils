{-| A hardware divider -}
module Clash.Arithmetic.Divide (
    divideStep,
    combDivide,
    nrDivideStep,
    combNRDivide
    ) where

import Clash.Prelude

{-| Perform one step of the restoring division algorithm -}
divideStep 
    :: forall n
    .  KnownNat n  
    => Vec n Bit        -- ^ Divisor
    -> Vec n Bit        -- ^ Divider state
    -> Bit              -- ^ Next msb
    -> (Vec n Bit, Bit) -- ^ (Output divider state, next bit of output)
divideStep divisor p b
    | divisor > shiftedP = (shiftedP,                0)
    | otherwise          = (vecSub shiftedP divisor, 1)
    where
    shiftedP = p <<+ b
    vecSub :: Vec n Bit -> Vec n Bit -> Vec n Bit
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

{-| Perform one step of the non-restoring division algorithm -}
nrDivideStep 
    :: forall n
    .  KnownNat n  
    => Vec (n + 1) Bit        -- ^ Divisor
    -> Vec (n + 1) Bit        -- ^ Divider state
    -> Bit                    -- ^ Next msb
    -> (Vec (n + 1) Bit, Bit) -- ^ (Output divider state, next bit of output)
nrDivideStep divisor p b = (res, complement $ head res)
    where
    msb :> rest = p :< b
    res = vecAddSub msb rest divisor
    vecAddSub :: Bit -> Vec (n + 1) Bit -> Vec (n + 1) Bit -> Vec (n + 1) Bit
    vecAddSub 0 x y = unpack $ pack x - pack y
    vecAddSub 1 x y = unpack $ pack x + pack y

restore
    :: KnownNat n
    => Vec (n + 1) Bit
    -> (Vec (n + 1) Bit, Vec (n + 1) Bit)
    -> (Vec (n + 1) Bit, Vec (n + 1) Bit)
restore divisor (r@(msb :> _), q) 
    | msb == 0 = (r, q)
    | msb == 1 = (unpack $ pack r + pack divisor, q)

{-| Perform non-restoring division combinationally. To achieve a high clock frequency with any reasonable sized division, you will probably want to use nrDivideStep and split up the computation into multiple clock cycles. -}
combNRDivide 
    :: forall n. KnownNat n 
    => BitVector (n + 1)                      -- ^ Number to be divided
    -> BitVector (n + 1)                      -- ^ Divisor
    -> (BitVector (n + 1), BitVector (n + 1)) -- ^ (quotient, remainder)
combNRDivide x y = (pack $ tail q, pack $ tail r)
    where
    x' = 0 :> unpack x
    y' = 0 :> unpack y
    (r, q) = restore y' $ mapAccumL (nrDivideStep y') (repeat 0) x'

