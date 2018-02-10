{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

{-| Bitvector serialization and deserialization using the Cereal library -}
module Clash.Serialize where

import Clash.Prelude
import Data.Serialize
import Data.Word
import Data.Foldable (sequenceA_)

-- | Deserialize
getBV :: KnownNat n => Get (BitVector ((n + 1) * 8))
getBV = fmap pack $ sequenceA $ repeat getWord8

-- | Serialize
putBV :: KnownNat n => Putter (BitVector ((n + 1) * 8))
putBV = sequenceA_ . map putWord8 . bitCoerce 

instance (KnownNat m, n ~ ((m + 1) * 8)) => Serialize (BitVector n) where
    put = putBV
    get = getBV

