{-# LANGUAGE QuasiQuotes #-}
{-| Xilinx carry chain primitives + Clash simulation models. See:

    * https://www.xilinx.com/support/documentation/sw_manuals/xilinx2012_2/ug953-vivado-7series-libraries.pdf, 
    * https://www.xilinx.com/support/documentation/sw_manuals/xilinx2018_1/ug974-vivado-ultrascale-libraries.pdf, and 
    * https://www.xilinx.com/support/documentation/user_guides/ug574-ultrascale-clb.pdf. 

    __FPGA proven__
-}
module Clash.Xilinx.Carry (
    carryN,
    carry8,
    carry4,
    xilinxCarryAdder
    ) where 

import Clash.Prelude
import Data.Bool
import Clash.Annotations.Primitive
import Data.String.Interpolate      (i)
import Data.String.Interpolate.Util (unindent)

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

-- | Xilinx Ultrascale(+) carry8 primitive. TODO: verilog/VHDL primitive definition.
carry8 = carryN @8

{-# ANN carry4 (InlinePrimitive Verilog $ unindent [i|
  [ { "BlackBox" :
      { "name" : "Clash.Xilinx.Carry.carry4"
      , "kind" : "Declaration"
      , "template" :
"
//CARRY4: Fast Carry Logic Component
//7 Series
//Xilinx HDL Libraries Guide, version 2012.2

wire [3:0] ~GENSYM[carry_res][0];
wire [3:0] ~GENSYM[cout][1];

CARRY4 CARRY4_inst(
    .CO(~SYM[1]),     //4-bit carry out
    .O(~SYM[0]),      //4-bit carry chain XOR data out
    .CI(~ARG[0]),     //1-bit carry cascade input
    .CYINIT(1'b0),    //1-bit carry initialization
    .DI(~ARG[1]),     //4-bit carry-MUX data in
    .S(~ARG[2])       //4-bit carry-MUX select input
);

assign ~RESULT = {~SYM[0], ~SYM[1]};

//End of CARRY4_inst instantiation
"
      }
    }
  ]
  |]) #-}
{-# NOINLINE carry4 #-}
-- | Xilinx Virtex 7 carry4 primitive 
carry4 = carryN @4

-- | An example adder built from a Xilinx carry chain
xilinxCarryAdder 
    :: KnownNat n 
    => Bool        -- ^ Carry in
    -> BitVector n -- ^ X
    -> BitVector n -- ^ Y 
    -> BitVector n -- ^ X + y
xilinxCarryAdder cIn x y = fst $ carryN cIn (x `xor` y) x

