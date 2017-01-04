{-# LANGUAGE ScopedTypeVariables #-}
module CLaSH.CRC where

import CLaSH.Prelude

import Data.Bool

serialCRC 
    :: forall n. (KnownNat (n + 1), KnownNat n)
    => BitVector (n + 1)          -- ^ Initial value of shift register
    -> BitVector n                -- ^ The polynomial
    -> Signal Bit                 -- ^ Input bit
    -> Signal (BitVector (n + 1)) -- ^ CRC
serialCRC init polynomial input = pack <$> shiftRegister
    where
    --The shift register
    shiftRegister :: Signal (Vec (n + 1) Bit) 
    shiftRegister =  register (unpack init) $ step <$> shiftRegister <*> input

    --The function which computes the next value of the shift register
    step :: Vec (n + 1) Bit -> Bit -> Vec (n + 1) Bit
    step (head :> rest) inp = zipWith selectIn (unpack polynomial) rest :< rightmostBit
        where
        rightmostBit = inp `xor` head
        selectIn sel bit =  bool bit (bit `xor` rightmostBit) sel

