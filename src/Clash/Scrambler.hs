{-| Multiplicative scrambler and descrambler: https://en.wikipedia.org/wiki/Scrambler. -}
module Clash.Scrambler (
    scramblerStep,
    scramblerSteps,
    serialScrambler,
    parallelScrambler,
    descramblerStep,
    descramblerSteps,
    serialDescrambler,
    parallelDescrambler
    ) where

import Clash.Prelude

scramblerStep 
    :: KnownNat n
    => BitVector n
    -> Vec (n + 1) Bool 
    -> Bool 
    -> (Vec (n + 1) Bool, Bool)
scramblerStep poly state input = (output +>> state, output)
    where
    output :: Bool
    output = foldl1 xor 
        $  zipWith (.&.) (init state) (unpack poly) 
        :< last state 
        :< input

scramblerSteps 
    :: forall n m. (KnownNat n, KnownNat m)
    => BitVector n
    -> Vec (n + 1) Bool
    -> Vec m Bool
    -> (Vec (n + 1) Bool, Vec m Bool)
scramblerSteps poly state input = mapAccumL (scramblerStep poly) state input

serialScrambler 
    :: forall dom n. (HiddenClockResetEnable dom, KnownNat n)
    => BitVector (n + 1) -- ^ Initial state
    -> BitVector n       -- ^ Polynomial
    -> Signal dom Bool   -- ^ Input bit
    -> Signal dom Bool   -- ^ Output bit
serialScrambler initial poly input = mealy (scramblerStep poly) (unpack initial) input

parallelScrambler 
    :: forall dom n m. (HiddenClockResetEnable dom, KnownNat n, KnownNat m)
    => BitVector (n + 1)       -- ^ Initial state
    -> BitVector n             -- ^ Polynomial
    -> Signal dom (Vec m Bool) -- ^ Input bits
    -> Signal dom (Vec m Bool) -- ^ Output bits
parallelScrambler initial poly input = mealy (scramblerSteps poly) (unpack initial) input

descramblerStep 
    :: KnownNat n
    => BitVector n
    -> Vec (n + 1) Bool 
    -> Bool 
    -> (Vec (n + 1) Bool, Bool)
descramblerStep poly state input = (input +>> state, output)
    where 
    output :: Bool
    output = foldl1 xor 
        $  zipWith (.&.) (init state) (unpack poly) 
        :< (last state) 
        :< input

descramblerSteps 
    :: forall n m. (KnownNat n, KnownNat m)
    => BitVector n
    -> Vec (n + 1) Bool
    -> Vec m Bool
    -> (Vec (n + 1) Bool, Vec m Bool)
descramblerSteps poly state input = mapAccumL (descramblerStep poly) state input

serialDescrambler 
    :: forall dom n. (HiddenClockResetEnable dom, KnownNat n)
    => BitVector (n + 1) -- ^ Initial state
    -> BitVector n       -- ^ Polynomial
    -> Signal dom Bool   -- ^ Input bit
    -> Signal dom Bool   -- ^ Output bit
serialDescrambler initial poly input = mealy (descramblerStep poly) (unpack initial) input

parallelDescrambler 
    :: forall dom n m. (HiddenClockResetEnable dom, KnownNat n, KnownNat m)
    => BitVector (n + 1)       -- ^ Initial state
    -> BitVector n             -- ^ Polynomial
    -> Signal dom (Vec m Bool) -- ^ Input bits
    -> Signal dom (Vec m Bool) -- ^ Output bits
parallelDescrambler initial poly input = mealy (descramblerSteps poly) (unpack initial) input

