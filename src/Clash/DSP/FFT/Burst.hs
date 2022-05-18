module Clash.DSP.FFT.Burst (
    burstFFT
) where

import Clash.Prelude

import Clash.Counter
import Clash.Misc
import Clash.DSP.FFT.Butterfly

--Generate addresses for both block rams such that for all butterfly strides, there will never be an address conflict
--Uses the fact that different bufferfly inputs/outputs always have different parity
--There might be a smarter way of doing this
addressGenerator
    :: forall dom nBits
    .  HiddenClockResetEnable dom
    => (KnownNat nBits, 1 <= nBits)
    => (
        Signal dom (Index (nBits + 1)), 
        Signal dom (Unsigned nBits), 
        Signal dom (Unsigned nBits), 
        Signal dom Bool, 
        Signal dom (Unsigned nBits),
        Signal dom (Unsigned nBits)
        )
addressGenerator = (stage, index, twiddleAddress, parity, address0, address1)
    where

    --
    --Top level counter for the FFT process
    --
    counter :: Signal dom (Unsigned (CLog 2 (nBits + 1) + nBits))
    counter = count 0 (pure True)

    --
    --Split the counter into stage + address
    --
    stage :: Signal dom (Index (nBits + 1))
    index :: Signal dom (Unsigned nBits)
    (stage, index) = unbundle $ bitCoerce <$> counter

    --
    --Calculate the twiddle factor address
    --
    twiddleAddress :: Signal dom (Unsigned nBits)
    twiddleAddress = shiftL <$> index <*> (fromIntegral <$> stage)

    --
    --Calculate parity
    --
    parity :: Signal dom Bool
    parity = foldr xor False <$> (bitCoerce <$> index)

    --
    --Calculate the address
    --
    base :: Vec (nBits + nBits) Bool
    base = replicate (SNat @nBits) True ++ replicate (SNat @nBits) False

    shifted :: Signal dom (Vec (nBits + nBits) Bool)
    shifted = rotateRight base <$> stage

    --Calculate the masks
    lowBits, highBits :: Signal dom (Vec nBits Bool)
    lowBits  = dropI <$> shifted 
    highBits = (False +>> ) . takeI <$> shifted

    address0 :: Signal dom (Unsigned nBits)
    address0 = func <$> highBits <*> lowBits <*> index
        where
        func highBits lowBits index = lowMasked .|. highMasked
            where
            lowMasked  = bitCoerce $ zipWith (&&) lowBits  (bitCoerce index)
            highMasked = bitCoerce $ zipWith (&&) highBits (False +>> bitCoerce index)

    highAddr :: Signal dom (Unsigned nBits)
    highAddr = unpack . revBV . shiftL 1 . fromIntegral <$> stage

    address1 :: Signal dom (Unsigned nBits)
    address1 = liftA2 (.|.) address0 highAddr

swap 
    :: Signal dom Bool
    -> (Signal dom a, Signal dom a)
    -> (Signal dom a, Signal dom a)
swap sel dat = unbundle $ swap' <$> sel <*> bundle dat
    where
    swap' False (x, y) = (x, y)
    swap' True  (x, y) = (y, x)

burstFFT
    :: forall dom nBits a
    .  HiddenClockResetEnable dom
    => (NFDataX a, BitPack a, Num a)
    => (KnownNat nBits, 1 <= nBits)
    => Vec (2 ^ nBits) a
    -> (Vec (2 ^ nBits) a, Vec (2 ^ nBits) a)
    -> (Signal dom a, Signal dom a)
burstFFT twiddles (dat0, dat1) = (butOut0, butOut1)
    where

    --
    --Address generator
    --
    (stage, index, twiddleAddress, parity, address0, address1) = addressGenerator 

    address0D = register 0 address0
    address1D = register 0 address1
    parityD   = register False parity

    --
    --The RAMs
    --

    --Select read address using parity
    (readAddr0, readAddr1) = swap parity (address0, address1)
    --Select read result using delayed parity
    (butIn0, butIn1) = swap parityD (readRes0, readRes1)
    --Select write value using delayed parity
    (write0, write1) = swap parityD (bundle (address0D, butOut0), bundle (address1D, butOut1))

    go :: Signal dom Bool
    go =  register False (pure True)

    readRes0 = blockRamPow2 dat0 readAddr0 (mux go (Just <$> write0) (pure Nothing))
    readRes1 = blockRamPow2 dat1 readAddr1 (mux go (Just <$> write1) (pure Nothing))

    --
    --The twiddle ROM
    --
    twiddle = romPow2 twiddles twiddleAddress 

    --
    --The butterfly
    --
    (butOut0, butOut1) = difButterfly twiddle butIn0 butIn1

