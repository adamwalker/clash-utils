{-| <https://en.wikipedia.org/wiki/Cyclic_redundancy_check Cyclic redundancy check> 
    
    This module provides several different ways of computing CRCs:

    * Serially, one bit at a time, with a linear feedback shift register: `serialCRC`, `crcStep`.

    * Several bits at a time, by unwinding the shift register algorithm n times: `parallelCRC`, `crcSteps`.

    * All at once using a table which exploits the linearity of the CRC: `makeCRCTable`, `crcTable`.

    * N bits at a time, also using a table which expolits the linearity of the CRC: `makeCRCTableMultiStep`, `crcTableMultiStep`.

    The table based methods are much more efficient.

    Each method is verified to be equivalent to the standard LFSR algorithm (the first one) in the testsuite.

    __FPGA proven__

-}
module Clash.ErrorControl.CRC (
    crc32Poly,
    crcStep,
    crcSteps,
    crcVerifyStep,
    crcVerifySteps,
    serialCRC,
    parallelCRC,
    makeCRCTable,
    crcTable,
    makeCRCTableMultiStep,
    crcTableMultiStep
    ) where

import Clash.Prelude
import Clash.LFSR.Feedback
import Clash.LFSR.Table

import Data.Bool

-- | The CRC32 polynomial. This is the one used by Ethernet.
crc32Poly :: BitVector 31
crc32Poly = 0b10011000001000111011011011

{-| Calculates CRC one bit / clock cycle -}
serialCRC 
    :: forall dom n. (HiddenClockResetEnable dom, KnownNat n)
    => BitVector (n + 1)              -- ^ Initial value of shift register
    -> BitVector n                    -- ^ The polynomial. The low order bit is assumed to be 1 so is not included.
    -> Signal dom Bool                -- ^ Input bit
    -> Signal dom (BitVector (n + 1)) -- ^ CRC
serialCRC init polynomial input = pack <$> mealy step' (unpack init) input
    where
    step' st inp = (x, x) where x = crcStep polynomial st inp

{-| Calculates CRC m bits / clock cycle -}
parallelCRC 
    :: forall dom n m. (HiddenClockResetEnable dom, KnownNat n, KnownNat m)
    => BitVector (n + 1)              -- ^ Initial value of shift register
    -> BitVector n                    -- ^ The polynomial. The low order bit is assumed to be 1 so is not included.
    -> Signal dom (BitVector m)       -- ^ Input bits
    -> Signal dom (BitVector (n + 1)) -- ^ CRC
parallelCRC init polynomial input = pack <$> mealy step' (unpack init) input
    where
    step' st inp = (x, x) where x = crcSteps polynomial st inp

{-| Shift one bit into the CRC shift register -}
crcStep 
    :: KnownNat n
    => BitVector n      -- ^ Polynomial. The low order bit is assumed to be 1 so is not included.
    -> Vec (n + 1) Bool -- ^ Shift register state
    -> Bool             -- ^ Input bit
    -> Vec (n + 1) Bool -- ^ Next shift register state
crcStep polynomial (head :> rest) inp = galoisFeedback polynomial rightmostBit rest :< rightmostBit
    where
    rightmostBit = inp `xor` head

{-| Shift m bits into the CRC shift register. You probably want to use `crcTable` instead. -}
crcSteps 
    :: forall n m. (KnownNat n, KnownNat m)
    =>  BitVector n     -- ^ Polynomial. The low order bit is assumed to be 1 so is not included.
    -> Vec (n + 1) Bool -- ^ Shift register state
    -> BitVector m      -- ^ Input bits
    -> Vec (n + 1) Bool -- ^ Next shift register state
crcSteps polynomial state input = foldl (crcStep polynomial) state (unpack input :: Vec m Bool)

{-| A modification of `crcStep` that does not xor each of the taps with the input bit. This means that, after the last bit of data has been shifted in, n + 1 0s must be shifted in to get the CRC. This is useful for verifying CRCs compute with `crcStep`. See the tests, specifically `prop_crc32_verify` for an example of this. -}
crcVerifyStep
    :: KnownNat n
    => BitVector n      -- ^ Polynomial. The low order bit is assumed to be 1 so is not included.
    -> Vec (n + 1) Bool -- ^ Shift register state
    -> Bool             -- ^ Input bit
    -> Vec (n + 1) Bool -- ^ Next shift register state
crcVerifyStep polynomial (head :> rest) inp = galoisFeedback polynomial head rest :< rightmostBit
    where
    rightmostBit = inp `xor` head

{-| A modification of `crcSteps` that does not xor each of the taps with the input bit. See `crcStep2`. You probably want to use `crcTable` instead. -}
crcVerifySteps
    :: forall n m. (KnownNat n, KnownNat m)
    => BitVector n      -- ^ Polynomial. The low order bit is assumed to be 1 so is not included.
    -> Vec (n + 1) Bool -- ^ Shift register state
    -> BitVector m      -- ^ Input bits
    -> Vec (n + 1) Bool -- ^ Next shift register state
crcVerifySteps polynomial state input = foldl (crcVerifyStep polynomial) state (unpack input :: Vec m Bool)

{-| Generates a table for use with `crcTable`. You may want to use template haskell to force the table to be evaluated at compile time. -}
makeCRCTable
    :: forall m n. KnownNat m
    => (BitVector m -> BitVector n) -- ^ CRC function that we are creating the table for
    -> Vec m (BitVector n)          -- ^ Resulting CRC table
makeCRCTable = makeLFSRTable

{-| Calculate the CRC using a table computed with `makeCRCTable`. This exploits the linearity of the CRC by computing the CRC for each bit position in the input BitVector and xoring the CRC's for each bit position that is a set in the input BitVector. This results in much shallower logic that simply unwinding the shift register as `crcSteps` and `crcSteps2` do. Consult the tests to see how it is used. -}
crcTable 
    :: forall m n. (KnownNat m, KnownNat n)
    => Vec (m + 1) (BitVector n) -- ^ CRC table generated by `makeCRCTable`
    -> BitVector (m + 1)         -- ^ Input bitvector to be CRC'd
    -> BitVector n               -- ^ CRC
crcTable = tableLFSR

{-| Generates the tables for use with `crcTableMultiStep`. You may want to use template haskell to force the tables to be evaluated at compile time. -}
makeCRCTableMultiStep
    :: forall n m. (KnownNat n, KnownNat m)
    => (BitVector n -> BitVector m -> BitVector n) -- ^ CRC function we are generating the tables for
    -> (Vec n (BitVector n), Vec m (BitVector n))  -- ^ Resulting CRC tables
makeCRCTableMultiStep func = 
    (
        makeCRCTable (flip func 0),
        makeCRCTable (func 0)
    )

{-| Calculate the CRC using a table computed with `makeCRCTableMultiStep`. This is the same as `crcTable` except that it allows the calculation to be split up in to multiple steps (and thus clock cycles) where n bits are checksummed in each step. Each step takes the current CRC shift register state as well as the bits to be checksummed. -}
crcTableMultiStep
    :: forall m n. (KnownNat m, KnownNat n)
    => Vec (n + 1) (BitVector (n + 1)) -- ^ CRC table generated by `makeCRCTableMultiStep`
    -> Vec (m + 1) (BitVector (n + 1)) -- ^ CRC table generated by `makeCRCTableMultiStep`
    -> BitVector (n + 1)               -- ^ Current shift register state
    -> BitVector (m + 1)               -- ^ Input bitvector to be CRC'd
    -> BitVector (n + 1)               -- ^ CRC
crcTableMultiStep shiftRegTable inputTable shiftReg input = (shiftRegComponent `xor` inputComponent)
    where
    shiftRegComponent = crcTable shiftRegTable (unpack shiftReg)
    inputComponent    = crcTable inputTable (unpack input)

