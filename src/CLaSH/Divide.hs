{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}

module CLaSH.Divide where

import CLaSH.Prelude

data Divide n = Divide {
    a        :: Vec n Bit,
    p        :: Vec n Bit,
    quotient :: Vec n Bit
}

initialDivide 
    :: KnownNat n 
    => BitVector n 
    -> Divide n
initialDivide dividend = Divide (unpack dividend) (repeat 0) (repeat 0)

divideStep 
    :: KnownNat (n + 1) 
    => Vec (n + 1) Bit 
    -> Divide (n + 1) 
    -> Divide (n + 1)
divideStep divisor Divide{..} 
    | divisor > shiftedP = Divide shiftedA shiftedP                  (quotient <<+ 0)
    | otherwise          = Divide shiftedA (vecSub shiftedP divisor) (quotient <<+ 1)
    where
    shiftedP = p <<+ head a
    shiftedA = a <<+ 0
    vecSub :: (KnownNat m) => Vec m Bit -> Vec m Bit -> Vec m Bit
    vecSub x y = unpack $ pack x - pack y

combDivide 
    :: forall n. KnownNat (n + 1) 
    => BitVector (n + 1) 
    -> BitVector (n + 1) 
    -> BitVector (n + 1)
combDivide x y = pack $ quotient $ last $ iterate ((snat :: SNat (n + 1)) `addSNat` d1) step init
    where
    init = initialDivide x
    step = divideStep (unpack y)

