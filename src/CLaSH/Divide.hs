{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}

{-| A hardware divider -}
module CLaSH.Divide where

import CLaSH.Prelude

import Control.Arrow

{-| The state between iterations of the division algorithm -}
data Divide n = Divide {
    a        :: Vec n Bit,
    p        :: Vec n Bit,
    quotient :: Vec n Bit
}

{-| Construct the initial divider state -}
initialDivide 
    :: KnownNat n 
    => BitVector n -- ^ Number to be divided
    -> Divide n    -- ^ Divider state
initialDivide dividend = Divide (unpack dividend) (repeat 0) (repeat 0)

{-| Perform one step of the restoring division algorithm -}
divideStep 
    :: KnownNat (n + 1) 
    => Vec (n + 1) Bit -- ^ Divisor
    -> Divide (n + 1)  -- ^ Divider state
    -> Divide (n + 1)  -- ^ Output divider state
divideStep divisor Divide{..} 
    | divisor > shiftedP = Divide shiftedA shiftedP                  (quotient <<+ 0)
    | otherwise          = Divide shiftedA (vecSub shiftedP divisor) (quotient <<+ 1)
    where
    shiftedP = p <<+ head a
    shiftedA = a <<+ 0
    vecSub :: (KnownNat m) => Vec m Bit -> Vec m Bit -> Vec m Bit
    vecSub x y = unpack $ pack x - pack y

{-| Perform division combinationally. To achieve a high clock frequency with any reasonable sized division, you will probably want to use divideStep and split up the computation into multiple clock cycles. -}
combDivide 
    :: forall n. KnownNat (n + 1) 
    => BitVector (n + 1)                      -- ^ Number to be divided
    -> BitVector (n + 1)                      -- ^ Divisor
    -> (BitVector (n + 1), BitVector (n + 1)) -- ^ (quotient, remainder)
combDivide x y = (pack . quotient &&& pack . p) $ last $ iterate ((SNat @ (n + 1)) `addSNat` d1) step init
    where
    init = initialDivide x
    step = divideStep (unpack y)

