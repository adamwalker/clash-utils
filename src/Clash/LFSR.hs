{-| Linear feedback shift registers.

    WARNING: this module has no tests so probably has bugs.
 -}
module Clash.LFSR (
    fibonacciLFSR,
    galoisLFSR
    ) where

import Clash.Prelude
import Data.Bool

-- | Fibonacci LFSR
fibonacciLFSR 
    :: KnownNat n
    => BitVector n       -- ^ Polynomial 
    -> Vec (n + 1) Bit   -- ^ Current state of shift register
    -> Vec (n + 1) Bit   -- ^ Next state of the shift register
fibonacciLFSR poly (head :> rest) = rest :< fold xor (head :> feedback)
    where
    feedback = zipWith (.&.) (unpack poly) rest

-- | Galois LFSR. Will result in more efficient hardware than the Fibonacci LFSR.
galoisLFSR 
    :: KnownNat n
    => BitVector n     -- ^ Polynomial 
    -> Vec (n + 1) Bit -- ^ Current state of shift register
    -> Vec (n + 1) Bit -- ^ Next state of the shift register
galoisLFSR poly (head :> rest) = zipWith selectIn (unpack poly) rest :< head
    where
    selectIn sel bit = bool bit (bit `xor` head) sel

lfsr 
    :: (HiddenClockResetEnable dom, KnownNat n) 
    => (Vec (n + 1) Bit -> Vec (n + 1) Bit) 
    -> BitVector (n + 1) 
    -> Signal dom Bit
lfsr step seed = msb <$> reg
    where 
    reg = register (unpack seed) (step <$> reg)

