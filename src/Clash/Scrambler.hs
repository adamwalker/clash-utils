{-| Multiplicative scrambler and descrambler: https://en.wikipedia.org/wiki/Scrambler. -}
module Clash.Scrambler (
    scramblerStep,
    serialScrambler,
    descramblerStep,
    serialDescrambler
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

serialScrambler 
    :: forall dom n. (HiddenClockResetEnable dom, KnownNat n)
    => BitVector (n + 1) -- ^ Initial state
    -> BitVector n       -- ^ Polynomial
    -> Signal dom Bool   -- ^ Input bit
    -> Signal dom Bool   -- ^ Output bit
serialScrambler initial poly input = mealy (scramblerStep poly) (unpack initial) input

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

serialDescrambler 
    :: forall dom n. (HiddenClockResetEnable dom, KnownNat n)
    => BitVector (n + 1) -- ^ Initial state
    -> BitVector n       -- ^ Polynomial
    -> Signal dom Bool   -- ^ Input bit
    -> Signal dom Bool   -- ^ Output bit
serialDescrambler initial poly input = mealy (descramblerStep poly) (unpack initial) input

