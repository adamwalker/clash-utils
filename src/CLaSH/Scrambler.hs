module CLaSH.Scrambler (
    scrambler,
    descrambler
    ) where

import CLaSH.Prelude

scrambler 
    :: forall n. KnownNat n
    => BitVector (n + 1)
    -> BitVector n 
    -> Signal Bool 
    -> Signal Bool
scrambler initial poly input = mealy scramblerStep (unpack initial) input
    where 
    scramblerStep :: Vec (n + 1) Bool -> Bool -> (Vec (n + 1) Bool, Bool)
    scramblerStep state input = (output +>> state, output)
        where
        output :: Bool
        output = foldl1 xor $ zipWith (.&.) (init state) (unpack poly) ++ singleton (last state) ++ singleton input

descrambler 
    :: forall n. KnownNat n
    => BitVector (n + 1)
    -> BitVector n
    -> Signal Bool
    -> Signal Bool
descrambler initial poly input = mealy descramblerStep (unpack initial) input
    where
    descramblerStep :: Vec (n + 1) Bool -> Bool -> (Vec (n + 1) Bool, Bool)
    descramblerStep state input = (input +>> state, foldl1 xor $ zipWith (.&.) (init state) (unpack poly) ++ singleton (last state) ++ singleton input)

