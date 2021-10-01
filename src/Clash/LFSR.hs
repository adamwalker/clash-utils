{-| Linear feedback shift registers.
 -}
module Clash.LFSR (
    serialLFSR,
    fibonacciLFSR,
    fibonacciLFSRStep,
    fibonacciLFSRSteps,
    serialFibonacciLFSR,
    galoisLFSR,
    galoisLFSRStep,
    galoisLFSRSteps,
    serialGaloisLFSR
    ) where

import Clash.Prelude
import Data.Bool
import Data.Tuple (swap)

serialLFSR
    :: forall dom n. (HiddenClockResetEnable dom, KnownNat n)
    => (Vec n Bit -> (Bit, Vec n Bit))
    -> Vec n Bit
    -> Signal dom Bit
serialLFSR step init = out
    where
    (out, shiftReg) 
        = unbundle 
        $ step <$> register init shiftReg

-- | Fibonacci LFSR
fibonacciLFSR 
    :: KnownNat n
    => BitVector n -- ^ Polynomial 
    -> Vec n Bit   -- ^ Current state of shift register
    -> Bit         -- ^ Next state of the shift register
fibonacciLFSR poly state = foldl xor 0 feedback
    where
    feedback = zipWith (.&.) (unpack poly) state

fibonacciLFSRStep
    :: KnownNat n
    => BitVector n
    -> Vec (n + 1) Bit
    -> (Bit, Vec (n + 1) Bit)
fibonacciLFSRStep poly (head :> rest) = (head, rest :< head `xor` feedback)
    where
    feedback = fibonacciLFSR poly rest

fibonacciLFSRSteps
    :: forall n m. (KnownNat n, KnownNat m)
    => BitVector n
    -> Vec (n + 1) Bit
    -> (Vec m Bit, Vec (n + 1) Bit)
fibonacciLFSRSteps poly state = swap $ mapAccumL func state (repeat ())
    where
    func state () = swap $ fibonacciLFSRStep poly state

serialFibonacciLFSR
    :: forall dom n. (HiddenClockResetEnable dom, KnownNat n)
    => BitVector n
    -> Vec (n + 1) Bit
    -> Signal dom Bit
serialFibonacciLFSR poly = serialLFSR (fibonacciLFSRStep poly)

-- | Galois LFSR. Will result in more efficient hardware than the Fibonacci LFSR.
galoisLFSR 
    :: KnownNat n
    => BitVector n -- ^ Polynomial 
    -> Bit
    -> Vec n Bit   -- ^ Current state of shift register
    -> Vec n Bit   -- ^ Next state of the shift register
galoisLFSR poly feedIn state = zipWith selectIn (unpack poly) state
    where
    selectIn sel bit = bool bit (bit `xor` feedIn) sel

galoisLFSRStep
    :: KnownNat n
    => BitVector n
    -> Vec (n + 1) Bit
    -> (Bit, Vec (n + 1) Bit)
galoisLFSRStep poly (head :> rest) = (head, galoisLFSR poly head rest :< head)

galoisLFSRSteps
    :: forall n m. (KnownNat n, KnownNat m)
    => BitVector n
    -> Vec (n + 1) Bit
    -> (Vec m Bit, Vec (n + 1) Bit)
galoisLFSRSteps poly state = swap $ mapAccumL func state (repeat ())
    where
    func state () = swap $ galoisLFSRStep poly state

serialGaloisLFSR
    :: forall dom n. (HiddenClockResetEnable dom, KnownNat n)
    => BitVector n
    -> Vec (n + 1) Bit
    -> Signal dom Bit
serialGaloisLFSR poly = serialLFSR (galoisLFSRStep poly)

