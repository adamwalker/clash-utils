module Clash.Misc(
    replaceSlice, 
    revBV,
    swapEndian, 
    mealyEn
    ) where

import Clash.Prelude

import Data.Bool

-- | Update a slice in a vector
replaceSlice
    :: forall m n a. (KnownNat n, KnownNat m)
    => Index n       -- ^ Index to start updating at
    -> Vec (m + 1) a -- ^ Slice to insert
    -> Vec n a       -- ^ Vector to update
    -> Vec n a       -- ^ Output vector
replaceSlice startIdx dat vec = imap func vec
    where
    func :: Index n -> a -> a
    func idx val
        | idx >= startIdx && resize idx <= (resize startIdx :: Index (m + n)) + snatToNum (SNat @ m)
            = dat !! (idx - startIdx)
        | otherwise 
            = val

-- | Reverse a bitvector
revBV :: forall n. KnownNat n => BitVector n -> BitVector n
revBV = pack . reverse . (unpack :: BitVector n -> Vec n Bit)

-- | Swap the endianness of a bitvector of bytes
swapEndian 
    :: forall n. KnownNat n
    => BitVector (8 * n)
    -> BitVector (8 * n)
swapEndian x = pack $ reverse bytes
    where
    bytes :: Vec n (BitVector 8)
    bytes = unpack x 

-- Same as mealy, but with an enable signal
mealyEn 
    :: HiddenClockReset dom gated sync
    => (s -> i -> (s, o)) -- ^ State update function
    -> s                  -- ^ Initial state
    -> Signal dom Bool    -- ^ Enable signal
    -> Signal dom i       -- ^ Input
    -> Signal dom o       -- ^ Output
mealyEn step initial enable input = mealy step' initial (bundle (enable, input))
    where
    step' state (enable, input) = (bool state state' enable, output)
        where (state', output) = step state input
