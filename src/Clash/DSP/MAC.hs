module Clash.DSP.MAC (
        MAC(..),
        MACPreAdd(..),
        macRealReal,
        macPreAddRealReal,
        macRealRealPipelined,
        macPreAddRealRealPipelined,
        macRealComplex,
        macPreAddRealComplex,
        macRealComplexPipelined,
        macPreAddRealComplexPipelined,
        macRealBool,
        macPreAddRealBool,
        macRealComplexBool,
        macPreAddRealComplexBool
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

-- | Real * Real multiply and accumulate with pre-add
macPreAddRealReal
    :: (KnownNat a, KnownNat b, KnownNat c) 
    => Signed a               -- ^ Real coefficient
    -> Signed b               -- ^ Real input
    -> Signed b               -- ^ Real input 2
    -> Signed (a + b + c + 1) -- ^ Real accumulator in
    -> Signed (a + b + c + 1) -- ^ Real accumulator out
macPreAddRealReal c i1 i2 b = extend (c `mul` (i1 `add` i2)) + b

-- | Real * Real multiply and accumulate. Designed to use the intermediate pipeline registers in Xilinx DSP48s.
macRealRealPipelined
    :: (HiddenClockResetEnable dom, KnownNat a, KnownNat b, KnownNat c) 
    => MAC dom (Signed a) (Signed b) (Signed (a + b + c))
macRealRealPipelined en c i a 
    = liftA2 (+) a
    $ fmap extend 
    $ delayEn (errorX "macRealRealPipelined post multiply register") en
    $ liftA2 mul c i

-- | Real * Real multiply and accumulate with pre-add. Designed to use the intermediate pipeline registers in Xilinx DSP48s.
macPreAddRealRealPipelined
    :: (HiddenClockResetEnable dom, KnownNat a, KnownNat b, KnownNat c) 
    => MACPreAdd dom (Signed a) (Signed b) (Signed (a + b + c + 1))
macPreAddRealRealPipelined en c i1 i2 a 
    = liftA2 (+) a
    $ fmap extend 
    $ delayEn (errorX "macPreAddRealRealPipelined post multiply register") en
    $ liftA2 mul (delayEn (errorX "initial coefficient") en c) --TODO: delay here is a waste of registers
    $ delayEn (errorX "macPreAddRealRealPipelined post pre-add register") en 
    $ liftA2 add i1 i2 

-- | Real * Complex multiply and accumulate
macRealComplex 
    :: (KnownNat a, KnownNat b, KnownNat c) 
    => Signed a                     -- ^ Real coefficient
    -> Complex (Signed b)           -- ^ Complex input
    -> Complex (Signed (a + b + c)) -- ^ Complex accumulator in
    -> Complex (Signed (a + b + c)) -- ^ Complex accumulator out
macRealComplex x = liftA2 (macRealReal x) 

-- | Real * Complex multiply and accumulate with pre-add
macPreAddRealComplex 
    :: (KnownNat a, KnownNat b, KnownNat c) 
    => Signed a                         -- ^ Real coefficient
    -> Complex (Signed b)               -- ^ Complex input
    -> Complex (Signed b)               -- ^ Complex input 2
    -> Complex (Signed (a + b + c + 1)) -- ^ Complex accumulator in
    -> Complex (Signed (a + b + c + 1)) -- ^ Complex accumulator out
macPreAddRealComplex c = liftA3 (macPreAddRealReal c) 

-- | Real * Complex multiply and accumulate. Designed to use the intermediate pipeline registers in Xilinx DSP48s.
macRealComplexPipelined 
    :: (HiddenClockResetEnable dom, KnownNat a, KnownNat b, KnownNat c) 
    => MAC dom (Signed a) (Complex (Signed b)) (Complex (Signed (a + b + c)))
macRealComplexPipelined en c i1 accum 
    = sequenceA 
    $ liftA2 (macRealRealPipelined en c) (sequenceA i1) (sequenceA accum)

-- | Real * Complex multiply and accumulate with pre add. Designed to use the intermediate pipeline registers in Xilinx DSP48s.
macPreAddRealComplexPipelined
    :: (HiddenClockResetEnable dom, KnownNat a, KnownNat b, KnownNat c) 
    => MACPreAdd dom (Signed a) (Complex (Signed b)) (Complex (Signed (a + b + c + 1)))
macPreAddRealComplexPipelined en c i1 i2 accum 
    = sequenceA 
    $ liftA3 (macPreAddRealRealPipelined en c) (sequenceA i1) (sequenceA i2) (sequenceA accum)

-- | MAC for 'multiplying' by a bool. Intended for digital modulation.
macRealBool 
    :: (KnownNat a, KnownNat c) 
    => Signed a       -- ^ Real coefficient
    -> Bool           -- ^ Real input
    -> Signed (a + c) -- ^ Real accumulator in
    -> Signed (a + c) -- ^ Real accumulator out
macRealBool x a b = b + toAdd
    where
    toAdd
        | a         = extend x
        | otherwise = negate $ extend x

-- | MAC for 'multiplying' by a bool. Intended for digital modulation. Pre-add version.
macPreAddRealBool
    :: (KnownNat a, KnownNat c) 
    => Signed a       -- ^ Real coefficient
    -> Bool           -- ^ Real input
    -> Bool           -- ^ Real input 2
    -> Signed (a + c) -- ^ Real accumulator in
    -> Signed (a + c) -- ^ Real accumulator out
macPreAddRealBool c i1 i2 b = b + toAdd
    where
    toAdd
        |     i1 &&     i2 = extend c
        | not i1 && not i2 = negate $ extend c
        | otherwise        = 0

-- | MAC for 'multiplying' by a complex bool. Intended for digital modulation.
macRealComplexBool
    :: (KnownNat a, KnownNat c) 
    => Signed a                 -- ^ Real coefficient
    -> Complex Bool             -- ^ Complex input
    -> Complex (Signed (a + c)) -- ^ Complex accumulator in
    -> Complex (Signed (a + c)) -- ^ Complex accumulator out
macRealComplexBool x = liftA2 (macRealBool x) 

-- | MAC for 'multiplying' by a complex bool. Intended for digital modulation. Pre-add version.
macPreAddRealComplexBool
    :: (KnownNat a, KnownNat c) 
    => Signed a                 -- ^ Real coefficient
    -> Complex Bool             -- ^ Complex input
    -> Complex Bool             -- ^ Complex input 2
    -> Complex (Signed (a + c)) -- ^ Complex accumulator in
    -> Complex (Signed (a + c)) -- ^ Complex accumulator out
macPreAddRealComplexBool c = liftA3 (macPreAddRealBool c) 

