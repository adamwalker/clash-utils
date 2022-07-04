module Clash.DSP.Fixed (
    renorm,
    truncateFrac,
    extendFrac,
    truncateInt
) where

import Clash.Prelude

{-
 - Functions for resizing and moving around the decimal point in fixed point numbers
 -}

-- | Adjust a signed fixed point number so that it's value ranges from [-1, +1).
renorm 
    :: forall n m
    . (KnownNat m, KnownNat n) 
    => SFixed (1 + n) m 
    -> SFixed 1 (n + m)
renorm = sf (SNat @(n + m)) . unSF 

-- | Decrease precision by dropping bits from the fractional part
truncateFrac 
    :: (KnownNat m, KnownNat n, KnownNat l) 
    => SFixed l (m + n) 
    -> SFixed l m
truncateFrac = resizeF --TODO: implement without relying on resizeF

-- | Increase precision by adding bits to the fractional part
extendFrac 
    :: (KnownNat m, KnownNat n, KnownNat l) 
    => SFixed l m
    -> SFixed l (m + n)
extendFrac = resizeF --TODO: implement without relying on resizeF

-- | Drop MSBs from the integer part of a fixed point number
truncateInt
    :: forall l m n
    .  (KnownNat m, KnownNat n, KnownNat l) 
    => SFixed (l + m) n
    -> SFixed m n
truncateInt = sf (SNat @n) . truncateB . unSF

