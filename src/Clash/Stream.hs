{-# LANGUAGE RecordWildCards #-}
{-| 
    __FPGA proven__
-}
module Clash.Stream (
    StreamIn(..),
    deserialize,
    selectStream,
    byteExtract,
    byteExtractAccum,
    fieldExtractAccum,
    byteExtractAccumComb,
    fieldExtractAccumComb
    ) where

import Clash.Prelude

data StreamIn a = StreamIn {
    sof   :: Bool,
    eof   :: Bool,
    valid :: Bool,
    dat   :: a
} deriving (Show)

deserialize 
    :: forall dom m a. (HiddenClockResetEnable dom, KnownNat m, Undefined a)
    => Signal dom (StreamIn a)      -- ^ Input data stream
    -> Signal dom (Maybe (Vec m a)) -- ^ Received data chunks
deserialize streamIn = mux lastDone (Just <$> buf) (pure Nothing)
    where

    inPkt'   :: Signal dom Bool
    inPkt'   =  register False (((sof <$> streamIn) .||. inPkt') .&&. (not . eof <$> streamIn))

    inPkt    :: Signal dom Bool
    inPkt    =  (sof <$> streamIn) .||. inPkt'

    lastDone :: Signal dom Bool
    lastDone =  register False (inPkt .&&. valid <$> streamIn .&&. ptr .==. pure maxBound)

    buf      :: Signal dom (Vec m a)
    buf      =  regEn (repeat (errorX "deserialize: initial vector")) (valid <$> streamIn) $ replace <$> ptr <*> (dat <$> streamIn) <*> buf

    ptr      :: Signal dom (Index m)
    ptr      =  mux (sof <$> streamIn) (pure 0) $ register 0 $ nextPtr <$> inPkt <*> ptr <*> streamIn
        where
        nextPtr inPkt ptr StreamIn{..}
            | eof             = 0
            | not valid       = ptr
            | ptr == maxBound = 0
            | inPkt           = ptr + 1
            | otherwise       = ptr

selectStream
    :: forall dom a. (HiddenClockResetEnable dom,  Eq a) 
    => (a -> Bool)
    -> Signal dom (StreamIn a)
    -> Signal dom (StreamIn a)
selectStream pred streamIn = StreamIn <$> register False match <*> (eof <$> streamIn) <*> (valid <$> streamIn) <*> (dat <$> streamIn)
    where
    match :: Signal dom Bool
    match =  (sof <$> streamIn) .&&. (pred . dat <$> streamIn)

----------------------------------------------------------
--Keep watch for an address
----------------------------------------------------------

byteExtract
    :: forall chunkSize lowBits highBits num. ( --These constraints are confusing and redundant, but seem to be required by the solver
        (highBits + lowBits) ~ BitSize num, highBits <= BitSize num, KnownNat highBits, 
        KnownNat (BitSize num), BitPack num, Num num, 
        KnownNat chunkSize)
    => num
    -> num
    -> Bool
    -> BitVector (chunkSize * (2 ^ lowBits))
    -> Maybe (BitVector chunkSize)
byteExtract offset ptr valid dat 
    | valid && ((pack high :: BitVector highBits) == 0) 
        = Just $ (unpack dat :: Vec (2 ^ lowBits) (BitVector chunkSize)) !! (pack low :: BitVector lowBits)
    | otherwise
        = Nothing
    where 
    diff = offset - ptr
    high :: Vec highBits Bit
    low  :: Vec lowBits  Bit
    (high, low) = splitAtI $ bitCoerce diff

----------------------------------------------------------
--Sequential versions
----------------------------------------------------------

byteExtractAccum 
    :: forall dom chunkSize lowBits (highBits :: Nat) num. (
        HiddenClockResetEnable dom, 
        highBits <= BitSize num, (highBits + lowBits) ~ BitSize num, KnownNat highBits, 
        KnownNat (BitSize num), BitPack num, Num num, 
        KnownNat chunkSize)
    => num
    -> Signal dom num
    -> Signal dom Bool
    -> Signal dom (BitVector (chunkSize * (2 ^ lowBits)))
    -> Signal dom (BitVector chunkSize)
byteExtractAccum offset ptr valid dat = regMaybe 0 $ byteExtract @chunkSize @lowBits @highBits @num offset <$> ptr <*> valid <*> dat

fieldExtractAccum
    :: forall dom chunkSize n lowBits (highBits :: Nat) num. (
        HiddenClockResetEnable dom, 
        highBits <= BitSize num, (highBits + lowBits) ~ BitSize num, KnownNat highBits, 
        KnownNat (BitSize num), BitPack num, Num num, 
        KnownNat chunkSize,
        KnownNat n, 1 <= n)
    => num
    -> Signal dom num
    -> Signal dom Bool
    -> Signal dom (BitVector (chunkSize * (2 ^ lowBits)))
    -> Signal dom (Vec n (BitVector chunkSize))
fieldExtractAccum offset ptr valid dat = sequenceA $ map (\offset -> byteExtractAccum offset ptr valid dat) offsets
    where 
    offsets :: Vec n num
    offsets = iterateI (+1) offset

----------------------------------------------------------
--Combinational versions
----------------------------------------------------------

byteExtractAccumComb
    :: forall dom chunkSize lowBits (highBits :: Nat) num. (
        HiddenClockResetEnable dom, 
        highBits <= BitSize num, (highBits + lowBits) ~ BitSize num, KnownNat highBits, 
        KnownNat (BitSize num), BitPack num, Num num, 
        KnownNat chunkSize)
    => num
    -> Signal dom num
    -> Signal dom Bool
    -> Signal dom (BitVector (chunkSize * (2 ^ lowBits)))
    -> Signal dom (BitVector chunkSize)
byteExtractAccumComb offset ptr valid dat = mealy func 0 $ byteExtract offset <$> ptr <*> valid <*> dat
    where
    func :: BitVector chunkSize -> Maybe (BitVector chunkSize) -> (BitVector chunkSize, BitVector chunkSize)
    func state Nothing  = (state, state)
    func state (Just x) = (x, x)

fieldExtractAccumComb
    :: forall dom chunkSize n lowBits (highBits :: Nat) num. (
        HiddenClockResetEnable dom, 
        highBits <= BitSize num, (highBits + lowBits) ~ BitSize num, KnownNat highBits, 
        KnownNat (BitSize num), BitPack num, Num num, 
        KnownNat chunkSize,
        KnownNat n, 1 <= n)
    => num
    -> Signal dom num
    -> Signal dom Bool
    -> Signal dom (BitVector (chunkSize * (2 ^ lowBits)))
    -> Signal dom (Vec n (BitVector chunkSize))
fieldExtractAccumComb offset ptr valid dat = sequenceA $ map (\offset -> byteExtractAccumComb offset ptr valid dat) offsets
    where 
    offsets :: Vec n num
    offsets = iterateI (+1) offset

