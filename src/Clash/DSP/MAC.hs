module Clash.DSP.MAC (
        MAC(..),
        MACPreAdd(..),
        macRealReal,
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
macRealComplexPipelined en c i1 accum = liftA2 (:+) a1 a2
    where
    m1 = fmap extend $ regEn 0 en $ liftA2 mul c (realPart <$> i1)
    m2 = fmap extend $ regEn 0 en $ liftA2 mul c (imagPart <$> i1)

    a1 = liftA2 (+) m1 (realPart <$> accum)
    a2 = liftA2 (+) m2 (imagPart <$> accum)

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
macPreAddRealComplexPipelined en c i1 i2 accum = liftA2 (:+) a3 a4
    where
    a1 = regEn 0 en $ liftA2 add (realPart <$> i1) (realPart <$> i2)
    a2 = regEn 0 en $ liftA2 add (imagPart <$> i1) (imagPart <$> i2)

    m1 = fmap extend $ regEn 0 en $ liftA2 mul c a1
    m2 = fmap extend $ regEn 0 en $ liftA2 mul c a2

    a3 = liftA2 (+) m1 (realPart <$> accum)
    a4 = liftA2 (+) m2 (imagPart <$> accum)

