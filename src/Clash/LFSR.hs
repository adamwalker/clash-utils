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
    => BitVector (n + 1) -- ^ Polynomial 
    -> Vec (n + 1) Bit   -- ^ Current state of shift register
    -> Vec (n + 1) Bit   -- ^ Next state of the shift register
fibonacciLFSR poly state = fold xor feedback +>> state
    where
    feedback = zipWith (.&.) (unpack poly) state

-- | Galois LFSR. Will result in more efficient hardware than the Fibonacci LFSR.
galoisLFSR 
    :: KnownNat n
    => BitVector (n + 1) -- ^ Polynomial 
    -> Vec (n + 1) Bit   -- ^ Current state of shift register
    -> Vec (n + 1) Bit   -- ^ Next state of the shift register
galoisLFSR poly state = zipWith selectIn (unpack poly) $ 0 +>> state
    where
    selectIn sel bit = bool bit (bit `xor` last state) sel

lfsr :: (HiddenClockResetEnable dom, KnownNat n) => (Vec (n + 1) Bit -> Vec (n + 1) Bit) -> BitVector (n + 1) -> Signal dom Bit
lfsr step seed = msb <$> reg
    where 
    reg = register (unpack seed) (step <$> reg)
