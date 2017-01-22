{-# LANGUAGE ScopedTypeVariables #-}

{-| <https://en.wikipedia.org/wiki/Cyclic_redundancy_check Cyclic redundancy check> -}
module CLaSH.CRC (
    crcStep,
    crcSteps,
    serialCRC,
    parallelCRC
    ) where

import CLaSH.Prelude

import Data.Bool

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

