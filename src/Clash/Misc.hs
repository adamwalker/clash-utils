module Clash.Misc(
    replaceSlice, 
    revBV,
    swapChunks, 
    swapEndian, 
    mealyEn,
    count,
    (++##),
    watchdog,
    setReset,
    wideWriteMem
    ) where

import Clash.Prelude

import Data.Bool

-- | Update a slice in a vector
replaceSlice
    :: forall m n a. (KnownNat n, KnownNat m)
    => Index n       -- ^ Index to start updating at
    -> Vec (m + 1) a -- ^ Slice to insert
    -> Vec n a       -- ^ Vector to update
    -> Vec n a       -- ^ Output vector
replaceSlice startIdx dat vec = imap func vec
    where
    func :: Index n -> a -> a
    func idx val
        | idx >= startIdx && resize idx <= (resize startIdx :: Index (m + n)) + snatToNum (SNat @ m)
            = dat !! (idx - startIdx)
        | otherwise 
            = val

-- | Reverse a bitvector
revBV :: forall n. KnownNat n => BitVector n -> BitVector n
revBV = pack . reverse . (unpack :: BitVector n -> Vec n Bit)

-- | Swap the endianness of a bitvector of bytes
swapChunks 
    :: forall n m. KnownNat n
    => SNat m
    -> BitVector (m * n)
    -> BitVector (m * n)
swapChunks SNat x = pack $ reverse bytes
    where
    bytes :: Vec n (BitVector m)
    bytes = unpack x 

swapEndian 
    :: forall n. KnownNat n
    => BitVector (8 * n)
    -> BitVector (8 * n)
swapEndian = swapChunks (SNat @8)

-- | Same as mealy, but with an enable signal
mealyEn 
    :: (HiddenClockResetEnable dom, Undefined s)
    => (s -> i -> (s, o)) -- ^ State update function
    -> s                  -- ^ Initial state
    -> Signal dom Bool    -- ^ Enable signal
    -> Signal dom i       -- ^ Input
    -> Signal dom o       -- ^ Output
mealyEn step initial enable input = mealy step' initial (bundle (enable, input))
    where
    step' state (enable, input) = (bool state state' enable, output)
        where (state', output) = step state input

-- | Counts cycles where the input signal is high
count 
    :: (HiddenClockResetEnable dom, Num a, Undefined a) 
    => Signal dom Bool -- ^ Increment signal
    -> Signal dom a    -- ^ Count output
count inc = res
    where
    res = regEn 0 inc $ res + 1

-- | Concatenates signals containing BitVectors
(++##) :: KnownNat m => Signal dom (BitVector n) -> Signal dom (BitVector m) -> Signal dom (BitVector (n + m))
(++##) = liftA2 (++#)

-- | A very simple watchdog timer
watchdog 
    :: (HiddenClockResetEnable dom, KnownNat n)
    => Unsigned n
    -> Signal dom Bool
    -> Signal dom Bool
watchdog touchCnt touch = res .==. 0
    where
    res = register 0 $ watchdog' <$> res <*> touch
    watchdog' res touch
        | touch     = touchCnt
        | res == 0  = 0
        | otherwise = res - 1

-- | Set-Reset flip flop. Reset has priority
setReset 
    :: forall dom. (HiddenClockResetEnable dom)
    => Signal dom Bool -- ^ Set
    -> Signal dom Bool -- ^ Reset
    -> Signal dom Bool -- ^ Result
setReset set reset = out
    where
    out  = register False $ func <$> out <*> set <*> reset
    func _ _    True = False
    func _ True _    = True
    func s _    _    = s

wideWriteMem 
  :: forall dom writeBits readBits bankBits a
  .  (HiddenClockResetEnable dom, KnownNat writeBits, KnownNat readBits, KnownNat bankBits, Undefined a)
  => (1 <= (2 ^ (readBits + bankBits))) --TODO: this constraint should be inferred
  => Vec (2 ^ (writeBits + readBits + bankBits)) a
  -> Signal dom (Unsigned (writeBits + readBits))
  -> Signal dom (Unsigned writeBits, Vec (2 ^ (readBits + bankBits)) (Maybe a))
  -> Signal dom (Vec (2 ^ bankBits) a)
wideWriteMem init raddr write = liftA2 (!!) readChunks (register 0 raddrBank)
    where

    raddrAddr :: Signal dom (BitVector writeBits)
    raddrBank :: Signal dom (BitVector readBits)
    (raddrAddr, raddrBank) = unbundle $ split <$> raddr

    writes :: Vec (2 ^ (readBits + bankBits)) (Signal dom (Maybe (Unsigned writeBits, a)))
    writes =  sequenceA $ func <$> write
        where
        func :: (Unsigned writeBits, Vec (2 ^ (readBits + bankBits)) (Maybe a)) -> Vec (2 ^ (readBits + bankBits)) (Maybe (Unsigned writeBits, a))
        func (wAddr, wValues) = map (fmap (wAddr, )) wValues

    ramReads :: Signal dom (Vec (2 ^ (readBits + bankBits)) a)
    ramReads = sequenceA $ zipWith3 blockRamPow2 (transpose $ unconcatI init) (repeat $ unpack <$> raddrAddr) writes 

    readChunks :: Signal dom (Vec (2 ^ readBits) (Vec (2 ^ bankBits) a))
    readChunks =  unconcatI <$> ramReads

