module CLaSH.Misc(
    replaceSlice, 
    revBV,
    swapEndian
    ) where

import CLaSH.Prelude

import Data.Bool

replaceSlice
    :: forall m n a. (KnownNat n, KnownNat m)
    => Index n
    -> Vec (m + 1) a
    -> Vec n a 
    -> Vec n a
replaceSlice startIdx dat vec = imap func vec
    where
    func :: Index n -> a -> a
    func idx val
        | idx >= startIdx && resize idx <= (resize startIdx :: Index (m + n)) + snatToNum (SNat @ m)
            = dat !! (idx - startIdx)
        | otherwise 
            = val

revBV :: forall n. KnownNat n => BitVector n -> BitVector n
revBV = pack . reverse . (unpack :: BitVector n -> Vec n Bit)

swapEndian 
    :: forall n. KnownNat n
    => BitVector (8 * n)
    -> BitVector (8 * n)
swapEndian x = pack $ reverse bytes
    where
    bytes :: Vec n (BitVector 8)
    bytes = unpack x 

mealyEn 
    :: (s -> i -> (s, o)) 
    -> s 
    -> Signal Bool 
    -> Signal i 
    -> Signal o
mealyEn step initial enable input = mealy step' initial (bundle (enable, input))
    where
    step' state (enable, input) = (bool state state' enable, output)
        where (state', output) = step state input
