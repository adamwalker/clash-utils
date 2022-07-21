{-| Linear feedback shift registers.

    __FPGA proven__
 -}
module Clash.LFSR.Feedback (
    fibonacciFeedback,
    galoisFeedback
    ) where

import Clash.Prelude
import Data.Bool
import Data.Tuple (swap)

-- | Fibonacci feedback
fibonacciFeedback 
    :: KnownNat n
    => BitVector n -- ^ Polynomial 
    -> Vec n Bool  -- ^ Current state of shift register
    -> Bool        -- ^ Next state of the shift register
fibonacciFeedback poly state = foldl xor False feedback
    where
    feedback = zipWith (.&.) (unpack poly) state

-- | Galois feedback. Will result in more efficient hardware than the Fibonacci LFSR.
galoisFeedback 
    :: KnownNat n
    => BitVector n -- ^ Polynomial 
    -> Bool
    -> Vec n Bool  -- ^ Current state of shift register
    -> Vec n Bool  -- ^ Next state of the shift register
galoisFeedback poly feedIn state = zipWith selectIn (unpack poly) state
    where
    selectIn sel bit = bool bit (bit `xor` feedIn) sel

