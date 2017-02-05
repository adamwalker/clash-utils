{-# LANGUAGE ScopedTypeVariables #-}

{-| <https://en.wikipedia.org/wiki/Cyclic_redundancy_check Cyclic redundancy check> -}
module CLaSH.CRC (
    crc32Poly,
    crcStep,
    crcSteps,
    crcStep2,
    crcSteps2,
    serialCRC,
    parallelCRC,
    makeCRCTable,
    crcTable
    ) where

import CLaSH.Prelude

import Data.Bool

--The CRC32 polynomial
crc32Poly :: BitVector 31
crc32Poly = 0b10011000001000111011011011

{-| Calculates CRC one bit / clock cycle -}
serialCRC 
    :: forall n. KnownNat n
    => BitVector (n + 1)          -- ^ Initial value of shift register
    -> BitVector n                -- ^ The polynomial. The low order bit is assumed to be 1 so is not included.
    -> Signal Bit                 -- ^ Input bit
    -> Signal (BitVector (n + 1)) -- ^ CRC
serialCRC init polynomial input = pack <$> mealy step' (unpack init) input
    where
    step' st inp = (x, x) where x = crcStep polynomial st inp

{-| Calculates CRC m bits / clock cycle -}
parallelCRC 
    :: forall n m. (KnownNat n, KnownNat m)
    => BitVector (n + 1)          -- ^ Initial value of shift register
    -> BitVector n                -- ^ The polynomial. The low order bit is assumed to be 1 so is not included.
    -> Signal (BitVector m)       -- ^ Input bit
    -> Signal (BitVector (n + 1)) -- ^ CRC
parallelCRC init polynomial input = pack <$> mealy step' (unpack init) input
    where
    step' st inp = (x, x) where x = crcSteps polynomial st inp

{-| Shift one bit into the CRC shift register -}
crcStep 
    :: KnownNat n
    => BitVector n     -- ^ Polynomial. The low order bit is assumed to be 1 so is not included.
    -> Vec (n + 1) Bit -- ^ Shift register state
    -> Bit             -- ^ Input bit
    -> Vec (n + 1) Bit -- ^ Next shift register state
crcStep polynomial (head :> rest) inp = zipWith selectIn (unpack polynomial) rest :< rightmostBit
    where
    rightmostBit = inp `xor` head
    selectIn sel bit =  bool bit (bit `xor` rightmostBit) sel

{-| Shift m bits into the CRC shift register -}
crcSteps 
    :: forall n m. (KnownNat n, KnownNat m)
    =>  BitVector n    -- ^ Polynomial. The low order bit is assumed to be 1 so is not included.
    -> Vec (n + 1) Bit -- ^ Shift register state
    -> BitVector m     -- ^ Input bits
    -> Vec (n + 1) Bit -- ^ Next shift register state
crcSteps polynomial state input = foldl (crcStep polynomial) state (unpack input :: Vec m Bit)

{-| A modification of `crcStep` that does not xor each of the taps with the input bit. This means that, after the last bit of data has been shifted in, n + 1 0s must be shifted in to get the CRC. This is useful for verifying CRCs compute with `crcStep`. See the tests, specifically `prop_crc32_verify` for an example of this. -}
crcStep2 
    :: KnownNat n
    => BitVector n     -- ^ Polynomial. The low order bit is assumed to be 1 so is not included.
    -> Vec (n + 1) Bit -- ^ Shift register state
    -> Bit             -- ^ Input bit
    -> Vec (n + 1) Bit -- ^ Next shift register state
crcStep2 polynomial (head :> rest) inp = zipWith selectIn (unpack polynomial) rest :< rightmostBit
    where
    rightmostBit = inp `xor` head
    selectIn sel bit =  bool bit (bit `xor` head) sel

{-| A modification of `crcSteps` that does not xor each of the taps with the input bit. See `crcStep2`. -}
crcSteps2
    :: forall n m. (KnownNat n, KnownNat m)
    =>  BitVector n    -- ^ Polynomial. The low order bit is assumed to be 1 so is not included.
    -> Vec (n + 1) Bit -- ^ Shift register state
    -> BitVector m     -- ^ Input bits
    -> Vec (n + 1) Bit -- ^ Next shift register state
crcSteps2 polynomial state input = foldl (crcStep2 polynomial) state (unpack input :: Vec m Bit)

makeCRCTable
    :: forall m n. KnownNat m
    => (BitVector m -> BitVector n)
    -> Vec m (BitVector n)
makeCRCTable func = map func $ reverse $ iterateI (`shiftL` 1) 1

crcTable 
    :: forall m n. (KnownNat m, KnownNat n)
    => Vec (m + 1) (BitVector n)
    -> BitVector (m + 1)
    -> BitVector n
crcTable table input = fold xor $ zipWith func (unpack input) table
    where
    func     :: Bit -> BitVector n -> BitVector n
    func x   =  pack . map (.&. x) . unpack 

