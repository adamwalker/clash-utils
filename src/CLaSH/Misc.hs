module CLaSH.Misc where

import CLaSH.Prelude

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
