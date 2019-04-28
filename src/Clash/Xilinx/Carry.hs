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

carryN
    :: forall n
    .  KnownNat n
    => Bool
    -> BitVector n
    -> BitVector n
    -> (BitVector n, BitVector n)
carryN cIn s d = (pack $ reverse $ zipWith xor (init c) sBits, pack $ reverse $ tail c)
    where

    sBits = reverse $ unpack s
    dBits = reverse $ unpack d

    c :: Vec (n + 1) Bool
    c = scanl carryStage cIn (zip sBits dBits)

    carryStage :: Bool -> (Bool, Bool) -> Bool
    carryStage cIn (s, d) = bool d cIn s

--TODO: primitive definition
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
    .CYINIT(~ARG[1]), //1-bit carry initialization
    .DI(~ARG[2]),     //4-bit carry-MUX data in
    .S(~ARG[3])       //4-bit carry-MUX select input
);

assign ~RESULT = {~SYM[1], ~SYM[0]};

//End of CARRY4_inst instantiation
"
      }
    }
  ]
  |]) #-}
{-# NOINLINE carry4 #-}
carry4 = carryN @4

xilinxCarryAdder :: KnownNat n => Bool -> BitVector n -> BitVector n -> BitVector n
xilinxCarryAdder cIn x y = fst $ carryN cIn (x `xor` y) x

