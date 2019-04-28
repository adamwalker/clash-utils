{-| Xilinx carry chain primitives + Clash simulation models. See:

    * https://www.xilinx.com/support/documentation/sw_manuals/xilinx2012_2/ug953-vivado-7series-libraries.pdf, 
    * https://www.xilinx.com/support/documentation/sw_manuals/xilinx2018_1/ug974-vivado-ultrascale-libraries.pdf, and 
    * https://www.xilinx.com/support/documentation/user_guides/ug574-ultrascale-clb.pdf. 
-}
module Clash.Xilinx.Carry (
    carryN,
    carry8,
    carry4,
    xilinxCarryAdder
    ) where 

import Clash.Prelude
import Data.Bool

-- | Length generic Xilinx carry chain model
carryN
    :: forall n
    .  KnownNat n
    => Bool                       -- ^ Carry in
    -> BitVector n                -- ^ S input
    -> BitVector n                -- ^ D input
    -> (BitVector n, BitVector n) -- ^ (Sum, carrys)
carryN cIn s d = (pack $ reverse $ zipWith xor (init c) sBits, pack $ reverse $ tail c)
    where

    sBits = reverse $ unpack s
    dBits = reverse $ unpack d

    c :: Vec (n + 1) Bool
    c = scanl carryStage cIn (zip sBits dBits)

    carryStage :: Bool -> (Bool, Bool) -> Bool
    carryStage cIn (s, d) = bool d cIn s

--TODO: primitive definitions for these

-- | Xilinx Ultrascale(+) carry8 primitive. TODO: verilog/VHDL primitive definition.
carry8 = carryN @8

-- | Xilinx Virtex 7 carry4 primitive. TODO: verilog/VHDL primitive definition.
carry4 = carryN @4

-- | An example adder built from a Xilinx carry chain
xilinxCarryAdder 
    :: KnownNat n 
    => Bool        -- ^ Carry in
    -> BitVector n -- ^ X
    -> BitVector n -- ^ Y 
    -> BitVector n -- ^ X + y
xilinxCarryAdder cIn x y = fst $ carryN cIn (x `xor` y) x

