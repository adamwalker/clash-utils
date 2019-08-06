module Clash.Arithmetic.Multiplier (
        multiply,
        multiplySigned,
        serialMultiplyCarrySave,
        serialMultiplyCarrySaveSigned
    ) where

import Clash.Prelude

import Data.Bool

import Clash.Arithmetic.CarrySave (fullAdder)

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

serialMultiplyCarrySave
    :: forall dom gated sync n 
    .  (HiddenClockReset dom gated sync, KnownNat n)
    => Signal dom (BitVector (n + 1))
    -> Signal dom Bool
    -> Signal dom Bool
    -> Signal dom Bool
serialMultiplyCarrySave x en y = sumOut
    where

    partials :: Vec (n + 1) (Signal dom Bool)
    partials =  sequenceA $ fmap unpack $ bool <$> 0 <*> x <*> y

    res :: Vec (n + 1) (Signal dom (Bool, Bool))
    res =  map (regEn (False, False) en) $ zipWith3 (liftA3 fullAdder) partials carryOuts (pure False :> sumOuts)

    sumOut  :: Signal dom Bool
    sumOuts :: Vec n (Signal dom Bool)
    sumOuts :< sumOut = map (fmap snd) res

    carryOuts :: Vec (n + 1) (Signal dom Bool)
    carryOuts = map (fmap fst) res

serialMultiplyCarrySaveSigned
    :: forall dom gated sync n 
    .  (HiddenClockReset dom gated sync, KnownNat n)
    => Signal dom (BitVector (n + 2))
    -> Signal dom Bool
    -> Signal dom Bool
    -> Signal dom Bool
    -> Signal dom Bool
serialMultiplyCarrySaveSigned x en msb y = sumOut
    where

    msbD :: Signal dom Bool
    msbD = regEn False en msb

    partials' :: Vec (n + 2) (Signal dom Bool)
    partials' =  sequenceA $ fmap unpack $ bool <$> 0 <*> x <*> y

    p :> ps = partials'

    finalConst :: Vec (n + 2) Bool
    finalConst = True :> (repeat False :< True)

    partials :: Vec (n + 2) (Signal dom Bool)
    partials 
        = zipWith (mux msbD) (map pure finalConst)
        $ map (liftA2 (bool id not) msb) $ fmap not p :> ps

    res :: Vec (n + 2) (Signal dom (Bool, Bool))
    res =  map (regEn (False, False) en) $ zipWith3 (liftA3 fullAdder) partials carryOuts (pure False :> sumOuts)

    sumOut  :: Signal dom Bool
    sumOuts :: Vec (n + 1) (Signal dom Bool)
    sumOuts :< sumOut = map (fmap snd) res

    carryOuts :: Vec (n + 2) (Signal dom Bool)
    carryOuts = map (fmap fst) res

