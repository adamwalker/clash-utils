module Clash.DSP.MAC (
        MAC(..),
        MACPreAdd(..),
        macRealReal,
        macRealRealPipelined,
        macPreAddRealRealPipelined,
        macRealComplex,
        macRealComplexPipelined,
        macPreAddRealComplex,
        macPreAddRealComplexPipelined
    )  where

import Clash.DSP.Complex
import Clash.Prelude

-- | Type of functions which multiply and accumulate
type MAC dom coeffType inputType outputType 
    =  Signal dom Bool 
    -> Signal dom coeffType 
    -> Signal dom inputType 
    -> Signal dom outputType 
    -> Signal dom outputType

-- | Type of functions which multiply and accumulate with pre-add
type MACPreAdd dom coeffType inputType outputType 
    =  Signal dom Bool 
    -> Signal dom coeffType 
    -> Signal dom inputType 
    -> Signal dom inputType 
    -> Signal dom outputType 
    -> Signal dom outputType

-- | Real * Real multiply and accumulate
macRealReal 
    :: (KnownNat a, KnownNat b, KnownNat c) 
    => Signed a           -- ^ Real coefficient
    -> Signed b           -- ^ Real input
    -> Signed (a + b + c) -- ^ Real accumulator in
    -> Signed (a + b + c) -- ^ Real accumulator out
macRealReal x a b = extend (x `mul` a) + b

-- | Real * Real multiply and accumulate. Designed to use the intermediate pipeline registers in Xilinx DSP48s.
macRealRealPipelined
    :: (HiddenClockResetEnable dom, KnownNat a, KnownNat b, KnownNat c) 
    => Signal dom Bool                 -- ^ Enable
    -> Signal dom (Signed a)           -- ^ Real coefficient
    -> Signal dom (Signed b)           -- ^ Real input
    -> Signal dom (Signed (a + b + c)) -- ^ Real accumulator in
    -> Signal dom (Signed (a + b + c)) -- ^ Real accumulator out
macRealRealPipelined en c i a 
    = liftA2 (+) a
    $ fmap extend 
    $ regEn 0 en
    $ liftA2 mul c i

-- | Real * Real multiply and accumulate with pre-add. Designed to use the intermediate pipeline registers in Xilinx DSP48s.
macPreAddRealRealPipelined
    :: (HiddenClockResetEnable dom, KnownNat a, KnownNat b, KnownNat c) 
    => Signal dom Bool                     -- ^ Enable
    -> Signal dom (Signed a)               -- ^ Real coefficient
    -> Signal dom (Signed b)               -- ^ Real input
    -> Signal dom (Signed b)               -- ^ Real input 2
    -> Signal dom (Signed (a + b + c + 1)) -- ^ Real accumulator in
    -> Signal dom (Signed (a + b + c + 1)) -- ^ Real accumulator out
macPreAddRealRealPipelined en c i1 i2 a 
    = liftA2 (+) a
    $ fmap extend 
    $ regEn 0 en
    $ liftA2 mul c
    $ regEn 0 en 
    $ liftA2 add i1 i2 

-- | Real * Complex multiply and accumulate
macRealComplex 
    :: (KnownNat a, KnownNat b, KnownNat c) 
    => Signed a                     -- ^ Real coefficient
    -> Complex (Signed b)           -- ^ Complex input
    -> Complex (Signed (a + b + c)) -- ^ Complex accumulator in
    -> Complex (Signed (a + b + c)) -- ^ Complex accumulator out
macRealComplex x (a1 :+ a2) (b1 :+ b2) = (extend (x `mul` a1) + b1) :+ (extend (x `mul` a2) + b2)

-- | Real * Complex multiply and accumulate. Designed to use the intermediate pipeline registers in Xilinx DSP48s.
macRealComplexPipelined 
    :: (HiddenClockResetEnable dom, KnownNat a, KnownNat b, KnownNat c) 
    => Signal dom Bool                           -- ^ Enable
    -> Signal dom (Signed a)                     -- ^ Real coefficient
    -> Signal dom (Complex (Signed b))           -- ^ Complex input
    -> Signal dom (Complex (Signed (a + b + c))) -- ^ Complex accumulator in
    -> Signal dom (Complex (Signed (a + b + c))) -- ^ Complex accumulator out
macRealComplexPipelined en c i1 accum 
    = sequenceA 
    $ liftA2 (macRealRealPipelined en c) (sequenceA i1) (sequenceA accum)

-- | Real * Complex multiply and accumulate with pre-add
macPreAddRealComplex 
    :: (KnownNat a, KnownNat b, KnownNat c) 
    => Signed a                         -- ^ Real coefficient
    -> Complex (Signed b)               -- ^ Complex input
    -> Complex (Signed b)               -- ^ Complex input 2
    -> Complex (Signed (a + b + c + 1)) -- ^ Complex accumulator in
    -> Complex (Signed (a + b + c + 1)) -- ^ Complex accumulator out
macPreAddRealComplex c (i11 :+ i12) (i21 :+ i22) (b1 :+ b2) = (extend (c `mul` a1) + b1) :+ (extend (c `mul` a2) + b2)
    where
    a1 = i11 `add` i21
    a2 = i12 `add` i22

-- | Real * Complex multiply and accumulate with pre add. Designed to use the intermediate pipeline registers in Xilinx DSP48s.
macPreAddRealComplexPipelined
    :: (HiddenClockResetEnable dom, KnownNat a, KnownNat b, KnownNat c) 
    => Signal dom Bool                               -- ^ Enable
    -> Signal dom (Signed a)                         -- ^ Real coefficient
    -> Signal dom (Complex (Signed b))               -- ^ Complex input
    -> Signal dom (Complex (Signed b))               -- ^ Complex input 2
    -> Signal dom (Complex (Signed (a + b + c + 1))) -- ^ Complex accumulator in
    -> Signal dom (Complex (Signed (a + b + c + 1))) -- ^ Complex accumulator out
macPreAddRealComplexPipelined en c i1 i2 accum 
    = sequenceA 
    $ liftA3 (macPreAddRealRealPipelined en c) (sequenceA i1) (sequenceA i2) (sequenceA accum)
