module Clash.Arithmetic.Multiplier (
        multiply,
        multiplySigned
    ) where

import Clash.Prelude

import Data.Bool

multiply 
    :: forall m n
    .  (KnownNat n, 1 <= n, KnownNat m)
    => BitVector m
    -> BitVector n
    -> BitVector (m + n)
multiply x y = sum toSum
    where
    toSum :: Vec n (BitVector (m + n))
    toSum =  imap func $ reverse $ unpack y
        where
        func :: Index n -> Bool -> BitVector (n + m)
        func idx bit = bool 0 (extend x `shiftL` fromIntegral idx) bit

multiplySigned
    :: forall m n
    .  (KnownNat n, KnownNat m)
    => BitVector (m + 1)
    -> BitVector (n + 1)
    -> BitVector (m + n + 2)
multiplySigned x y = sum $ 
           (1 `shiftL` snatToNum (SNat @ (m + n + 1))) 
        :> (1 `shiftL` snatToNum (SNat @ (m + 1))) 
        :> toSum
    where

    toSum :: Vec (n + 1) (BitVector (m + n + 2))
    toSum = imap func $ reverse $ unpack y
        where
        func idx0 bit = flip shiftL (fromIntegral idx0) $ extend $ pack $ reverse $ imap func2 (reverse $ unpack $ bool 0 x bit)
            where
            func2 :: Index (m + 1) -> Bool -> Bool
            func2 idx1 val = (idx0 == maxBound) `xor` (idx1 == maxBound) `xor` val

