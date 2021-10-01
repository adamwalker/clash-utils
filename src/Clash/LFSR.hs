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
    => (Vec n Bool -> (Bool, Vec n Bool))
    -> Vec n Bool
    -> Signal dom Bool
serialLFSR step init = out
    where
    (out, shiftReg) 
        = unbundle 
        $ step <$> register init shiftReg

-- | Fibonacci LFSR
fibonacciLFSR 
    :: KnownNat n
    => BitVector n -- ^ Polynomial 
    -> Vec n Bool  -- ^ Current state of shift register
    -> Bool        -- ^ Next state of the shift register
fibonacciLFSR poly state = foldl xor False feedback
    where
    feedback = zipWith (.&.) (unpack poly) state

fibonacciLFSRStep
    :: KnownNat n
    => BitVector n
    -> Vec (n + 1) Bool
    -> (Bool, Vec (n + 1) Bool)
fibonacciLFSRStep poly (head :> rest) = (head, rest :< head `xor` feedback)
    where
    feedback = fibonacciLFSR poly rest

fibonacciLFSRSteps
    :: forall n m. (KnownNat n, KnownNat m)
    => BitVector n
    -> Vec (n + 1) Bool
    -> (Vec m Bool, Vec (n + 1) Bool)
fibonacciLFSRSteps poly state = swap $ mapAccumL func state (repeat ())
    where
    func state () = swap $ fibonacciLFSRStep poly state

serialFibonacciLFSR
    :: forall dom n. (HiddenClockResetEnable dom, KnownNat n)
    => BitVector n
    -> Vec (n + 1) Bool
    -> Signal dom Bool
serialFibonacciLFSR poly = serialLFSR (fibonacciLFSRStep poly)

-- | Galois LFSR. Will result in more efficient hardware than the Fibonacci LFSR.
galoisLFSR 
    :: KnownNat n
    => BitVector n -- ^ Polynomial 
    -> Bool
    -> Vec n Bool  -- ^ Current state of shift register
    -> Vec n Bool  -- ^ Next state of the shift register
galoisLFSR poly feedIn state = zipWith selectIn (unpack poly) state
    where
    selectIn sel bit = bool bit (bit `xor` feedIn) sel

galoisLFSRStep
    :: KnownNat n
    => BitVector n
    -> Vec (n + 1) Bool
    -> (Bool, Vec (n + 1) Bool)
galoisLFSRStep poly (head :> rest) = (head, galoisLFSR poly head rest :< head)

galoisLFSRSteps
    :: forall n m. (KnownNat n, KnownNat m)
    => BitVector n
    -> Vec (n + 1) Bool
    -> (Vec m Bool, Vec (n + 1) Bool)
galoisLFSRSteps poly state = swap $ mapAccumL func state (repeat ())
    where
    func state () = swap $ galoisLFSRStep poly state

serialGaloisLFSR
    :: forall dom n. (HiddenClockResetEnable dom, KnownNat n)
    => BitVector n
    -> Vec (n + 1) Bool
    -> Signal dom Bool
serialGaloisLFSR poly = serialLFSR (galoisLFSRStep poly)

