{-| Multipliers. Don't bother with these if your FPGA has hard DSP/multiplier blocks. -}
module Clash.Arithmetic.Multiplier (
        multiply,
        multiplySigned,
        serialMultiplyCarrySave,
        serialMultiplyCarrySaveSigned,
        BoothRes(..),
        boothEncode,
        boothWindows,
        boothMultiply
    ) where

import Clash.Prelude

import Data.Bool

import Clash.Arithmetic.Adder.CarrySave (fullAdder)

-- | Basic purely combinational unsigned*unsigned multiply
multiply 
    :: forall m n
    .  (KnownNat n, 1 <= n, KnownNat m)
    => BitVector m -- ^ x
    -> BitVector n -- ^ y
    -> BitVector (m + n) -- ^ x * y
multiply x y = sum toSum
    where
    toSum :: Vec n (BitVector (m + n))
    toSum =  imap func $ reverse $ unpack y
        where
        func :: Index n -> Bool -> BitVector (n + m)
        func idx bit = bool 0 (extend x `shiftL` fromIntegral idx) bit

-- | Basic purely combinational signed*signed multiply
multiplySigned
    :: forall m n
    .  (KnownNat n, KnownNat m)
    => BitVector (m + 1) -- ^ x
    -> BitVector (n + 1) -- ^ y
    -> BitVector (m + n + 2) -- ^ x * y
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

-- | Serial unsigned carry save multiplier. One of the inputs is fed in serially and the result is computed over several cycles. Since a carry save adder is used, the combinational delay is very small.
serialMultiplyCarrySave
    :: forall dom n 
    .  (HiddenClockResetEnable dom, KnownNat n)
    => Signal dom (BitVector (n + 1)) -- ^ x
    -> Signal dom Bool                -- ^ y input valid
    -> Signal dom Bool                -- ^ y serial input
    -> Signal dom Bool                -- ^ x * y serial output
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

-- | Serial signed carry save multiplier. One of the inputs is fed in serially and the result is computed over several cycles. Since a carry save adder is used, the combinational delay is very small.
serialMultiplyCarrySaveSigned
    :: forall dom n 
    .  (HiddenClockResetEnable dom, KnownNat n)
    => Signal dom (BitVector (n + 2)) -- ^ x
    -> Signal dom Bool                -- ^ y input valid
    -> Signal dom Bool                -- ^ y serial input
    -> Signal dom Bool                -- ^ High on MSB of y
    -> Signal dom Bool                -- ^ x * y serial output
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

-- | Result of [Booth encoding](https://en.wikipedia.org/wiki/Booth%27s_multiplication_algorithm) three bits
data BoothRes = BoothRes {
    mult   :: Bool,
    shift  :: Bool,
    negate :: Bool
} deriving (Show)

-- | Perform [Booth encoding](https://en.wikipedia.org/wiki/Booth%27s_multiplication_algorithm) 
boothEncode 
    :: BitVector 3 -- ^ Input 3 bit vector
    -> BoothRes    -- ^ Booth encoding result
boothEncode 0x0 = BoothRes False False False
boothEncode 0x1 = BoothRes True  False False
boothEncode 0x2 = BoothRes True  False False
boothEncode 0x3 = BoothRes True  True  False
boothEncode 0x4 = BoothRes True  True  True
boothEncode 0x5 = BoothRes True  False True
boothEncode 0x6 = BoothRes True  False True
boothEncode 0x7 = BoothRes False False True

-- | Extract 3 bit chunks from a bit vector for [Booth encoding](https://en.wikipedia.org/wiki/Booth%27s_multiplication_algorithm) 
boothWindows 
    :: forall n
    .  KnownNat n
    => Vec (2 * (n + 1)) Bool   -- ^ Input bit vector
    -> Vec (n + 1) (Vec 3 Bool) -- ^ 3 bit windows
boothWindows xs = map (take (SNat @ 3)) $ map (:< False) shifts
    where

    shiftL :: Vec (2 * (n + 1)) Bool -> Vec (2 * (n + 1)) Bool
    shiftL ys = drop (SNat @ 2) ys :< False :< False

    shifts :: Vec (n + 1) (Vec (2 * (n + 1)) Bool)
    shifts = iterateI shiftL xs

-- | Unsigned multiplication using [Booths algorithm](https://en.wikipedia.org/wiki/Booth%27s_multiplication_algorithm) 
boothMultiply
    :: forall n m
    .  (KnownNat m, KnownNat n) 
    => BitVector m                   -- ^ x
    -> BitVector ((2 * n) + 1)       -- ^ y
    -> BitVector (m + (2 * (n + 1))) -- ^ x * y
boothMultiply x y = ifoldl (sumFunc x) 0 $ reverse res
    where
    res :: Vec (n + 1) BoothRes
    res =  map (boothEncode . pack) $ boothWindows (False :> unpack y :: Vec (2 * (n + 1)) Bool)

    sumFunc x accum i (BoothRes mult shift negate) =
        let
            shifted
                | shift     = extend x `shiftL` 1
                | otherwise = extend x
            summed 
                | negate    = accum - (shifted `shiftL` (2 * fromIntegral i))
                | otherwise = accum + (shifted `shiftL` (2 * fromIntegral i))
            noped
                | mult      = summed
                | otherwise = accum
        in noped

