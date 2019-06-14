{-# LANGUAGE ScopedTypeVariables #-}

{-| FIR filters: see <http://adamwalker.github.io/Filter-Design-in-Clash/>.

    These are based on designs in the Xilinx document <http://www-inst.eecs.berkeley.edu/~cs150/fa13/resources/dsp.pdf DSP: Designing for Optimal Results>
-}
module Clash.DSP.FIRFilter (
    MAC(..),
    MACPreAdd(..),
    macRealComplex,
    macRealComplexPipelined,
    macPreAddRealComplex,
    macPreAddRealComplexPipelined,
    fir,
    firTransposed,
    firSystolic,
    firSymmetric,
    firSystolicSymmetric,
    firSystolicSymmetricOdd,
    firSystolicHalfBand,
    semiParallelFIR
    ) where

import Clash.Prelude

import Clash.DSP.Complex

--Multiply and accumulate functions

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
    :: (HiddenClockReset dom gated sync, KnownNat a, KnownNat b, KnownNat c) 
    => Signal dom Bool                           -- ^ Enable
    -> Signal dom (Signed a)                     -- ^ Real coefficient
    -> Signal dom (Complex (Signed b))           -- ^ Complex input
    -> Signal dom (Complex (Signed (a + b + c))) -- ^ Complex accumulator in
    -> Signal dom (Complex (Signed (a + b + c))) -- ^ Complex accumulator out
macRealComplexPipelined en c i1 accum = liftA2 (:+) a1 a2
    where
    m1 = fmap extend $ regEn 0 en $ liftA2 mul' c (realPart <$> i1)
    m2 = fmap extend $ regEn 0 en $ liftA2 mul' c (imagPart <$> i1)

    a1 = liftA2 (+) m1 (realPart <$> accum)
    a2 = liftA2 (+) m2 (imagPart <$> accum)

    --Work around Clash issue #601
    mul' :: (KnownNat a, KnownNat b) => Signed a -> Signed b -> Signed (a + b)
    mul' a b = resize a * resize b

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
    :: (HiddenClockReset dom gated sync, KnownNat a, KnownNat b, KnownNat c) 
    => Signal dom Bool                               -- ^ Enable
    -> Signal dom (Signed a)                         -- ^ Real coefficient
    -> Signal dom (Complex (Signed b))               -- ^ Complex input
    -> Signal dom (Complex (Signed b))               -- ^ Complex input 2
    -> Signal dom (Complex (Signed (a + b + c + 1))) -- ^ Complex accumulator in
    -> Signal dom (Complex (Signed (a + b + c + 1))) -- ^ Complex accumulator out
macPreAddRealComplexPipelined en c i1 i2 accum = liftA2 (:+) a3 a4
    where
    a1 = regEn 0 en $ liftA2 add' (realPart <$> i1) (realPart <$> i2)
    a2 = regEn 0 en $ liftA2 add' (imagPart <$> i1) (imagPart <$> i2)

    m1 = fmap extend $ regEn 0 en $ liftA2 mul' c a1
    m2 = fmap extend $ regEn 0 en $ liftA2 mul' c a2

    a3 = liftA2 (+) m1 (realPart <$> accum)
    a4 = liftA2 (+) m2 (imagPart <$> accum)

    --Work around Clash issue #601
    add' :: (KnownNat a, KnownNat b) => Signed a -> Signed b -> Signed ((Max a b) + 1)
    add' a b = resize a + resize b

    mul' :: (KnownNat a, KnownNat b) => Signed a -> Signed b -> Signed (a + b)
    mul' a b = resize a * resize b

--Filters

{- | Direct form FIR filter -}
fir 
    :: (HiddenClockReset dom gated sync, KnownNat n, Undefined inputType, Num inputType) 
    => (Signal dom coeffType  -> Signal dom inputType  -> Signal dom outputType) -- ^ Function to do the multiplication
    -> (Signal dom outputType -> Signal dom outputType -> Signal dom outputType) -- ^ Function to do the accumulation
    -> Vec (n + 1) coeffType                                                     -- ^ Coefficients
    -> Signal dom Bool                                                           -- ^ Input enable
    -> Signal dom inputType                                                      -- ^ Input samples
    -> Signal dom outputType                                                     -- ^ Output samples
fir mul add coeffs en x = dotp (map pure coeffs) (iterateI (regEn 0 en) x)
    where
    dotp as bs = fold add $ zipWith mul as bs

-- | Type of functions which multiply and accumulate
type MAC dom coeffType inputType outputType 
    =  Signal dom Bool 
    -> Signal dom coeffType 
    -> Signal dom inputType 
    -> Signal dom outputType 
    -> Signal dom outputType

{- | Transposed FIR filter -}
firTransposed 
    :: (HiddenClockReset dom gated sync, KnownNat n, Undefined outputType, Num outputType) 
    => MAC dom coeffType inputType outputType -- ^ Function to do the multiply and accumulate
    -> Vec (n + 1) coeffType                  -- ^ Coefficients
    -> Signal dom Bool                        -- ^ Input enable
    -> Signal dom inputType                   -- ^ Input samples
    -> Signal dom outputType                  -- ^ Output samples
firTransposed mac coeffs en x = foldl (func x) 0 $ (pure <$> coeffs)
    where
    func x accum coeff = regEn 0 en $ mac en coeff x accum

{- | Systolic FIR filter -}
firSystolic 
    :: (HiddenClockReset dom gated sync, KnownNat n, Undefined outputType, Num outputType, Num inputType, Undefined inputType) 
    => MAC dom coeffType inputType outputType -- ^ Function to do the multiply and accumulate
    -> Vec (n + 1) coeffType                  -- ^ Coefficients
    -> Signal dom Bool                        -- ^ Input enable
    -> Signal dom inputType                   -- ^ Input samples
    -> Signal dom outputType                  -- ^ Output samples
firSystolic mac coeffs en x = foldl func 0 $ zip (map pure coeffs) $ iterateI (regEn 0 en . regEn 0 en) x
    where
    func accum (coeff, input) = regEn 0 en $ mac en coeff input accum

-- | Type of functions which multiply and accumulate with pre-add
type MACPreAdd dom coeffType inputType outputType 
    =  Signal dom Bool 
    -> Signal dom coeffType 
    -> Signal dom inputType 
    -> Signal dom inputType 
    -> Signal dom outputType 
    -> Signal dom outputType

--TODO: symmetric odd and Symmetric half band
{- | Symmetric FIR filter -}
firSymmetric
    :: (HiddenClockReset dom gated sync, KnownNat n, Num inputType, Undefined inputType, Num outputType, Undefined outputType) 
    => MACPreAdd dom coeffType inputType outputType -- ^ Function to do the multiply and accumulate
    -> Vec (n + 1) coeffType                        -- ^ Coefficients
    -> Signal dom Bool                              -- ^ Input enable
    -> Signal dom inputType                         -- ^ Input samples
    -> Signal dom outputType                        -- ^ Output samples
firSymmetric macPreAdd coeffs en x = foldl (func x) 0 $ zip (pure <$> coeffs) delayed 
    where
    func x accum (coeff, y) = regEn 0 en $ macPreAdd en coeff x y accum
    delayed                 = iterate (lengthS coeffs) (regEn 0 en . regEn 0 en) (regEn 0 en x)

{- | Systolic Symmetric FIR filter with even number of coefficients -}
firSystolicSymmetric
    :: (HiddenClockReset dom gated sync, KnownNat n, Num outputType, Undefined outputType, Num inputType, Undefined inputType) 
    => MACPreAdd dom coeffType inputType outputType -- ^ Function to do the multiply and accumulate with pre-add
    -> Vec (n + 1) coeffType                        -- ^ Coefficients
    -> Signal dom Bool                              -- ^ Input enable
    -> Signal dom inputType                         -- ^ Input samples
    -> Signal dom outputType                        -- ^ Output samples
firSystolicSymmetric macPreAdd coeffs en x = foldl (func lastDelayLine) 0 $ zip (map pure coeffs) delayLine
    where
    delayLine                      = iterateI (regEn 0 en . regEn 0 en) x
    lastDelayLine                  = regEn 0 en $ last delayLine
    func last accum (coeff, input) = regEn 0 en $ macPreAdd en coeff last input accum

{- | Systolic symmetric FIR filter with odd number of coefficients -}
firSystolicSymmetricOdd
    :: (HiddenClockReset dom gated sync, KnownNat n, Num outputType, Undefined outputType, Num inputType, Undefined inputType) 
    => MACPreAdd dom coeffType inputType outputType -- ^ Function to do the multiply and accumulate with pre-add
    -> Vec (n + 2) coeffType                        -- ^ Coefficients
    -> Signal dom Bool                              -- ^ Input enable
    -> Signal dom inputType                         -- ^ Input samples
    -> Signal dom outputType                        -- ^ Output samples
firSystolicSymmetricOdd macPreAdd coeffs en x = foldl (func lastDelayLine) 0 $ zip (map pure coeffs) $ delayLine ++ singleton 0
    where
    delayLine                      = iterateI (regEn 0 en . regEn 0 en) x
    lastDelayLine                  = regEn 0 en $ regEn 0 en $ last delayLine
    func last accum (coeff, input) = regEn 0 en $ macPreAdd en coeff last input accum

{- | Systolic half band filter (also symmetric and odd number of coefficients) -}
firSystolicHalfBand
    :: (HiddenClockReset dom gated sync, KnownNat n, Num inputType, Num outputType, Undefined inputType, Undefined outputType) 
    => MACPreAdd dom coeffType inputType outputType -- ^ Function to do the multiply and accumulate with pre-add
    -> Vec (n + 2) coeffType                        -- ^ Coefficients
    -> Signal dom Bool                              -- ^ Input enable
    -> Signal dom inputType                         -- ^ Input samples
    -> Signal dom outputType                        -- ^ Output samples
firSystolicHalfBand macPreAdd coeffs en x = foldl func 0 $ zip3 (map pure coeffs) (delayLine ++ singleton 0) (reverse delayedReturn ++ singleton lastDelayLine) 
    where
    delayLine                       = iterateI (regEn 0 en . regEn 0 en . regEn 0 en) x
    lastDelayLine                   = regEn 0 en $ regEn 0 en $ last delayLine
    delayedReturn                   = iterateI (regEn 0 en) lastDelayLine
    func accum (coeff, input, last) = regEn 0 en $ macPreAdd en coeff last input accum 

semiParallelFIR 
    :: forall dom gated sync a n m n' m'. (HiddenClockReset dom gated sync, Num a, KnownNat n, KnownNat m, n ~ (n' + 1), m ~ (m' + 1), Undefined a)
    => Vec n (Vec m a)
    -> Signal dom Bool
    -> Signal dom a
    -> Signal dom a
semiParallelFIR coeffs en x = accum
--semiParallelFIR coeffs en x = bundle $ (stepChunk, sequenceA inputChunks, sequenceA addresses, sequenceA currentSamples, outputStream, dumpIt, accum)
    where

    --Hopefully this will be implemented in SRL16s
    inputChunks :: Vec n (Signal dom (Vec m a))
    inputChunks = zipWith (regEn (repeat 0)) stepChunks $ zipWith (liftA2 (+>>)) lastInChunk inputChunks 

    lastInChunk :: Vec n (Signal dom a)
    lastInChunk = x +>> currentSamples 

    address :: Signal dom (Index m)
    address = regEn 0 en (wrappingInc <$> address)
        where
        wrappingInc x
            | x == maxBound = 0
            | otherwise     = x + 1

    stepChunk :: Signal dom Bool
    stepChunk = (address .==. 0) .&&. en

    stepChunks :: Vec n (Signal dom Bool)
    stepChunks = iterateI (regEn False en) stepChunk

    addresses :: Vec n (Signal dom (Index m))
    addresses = tail $ iterateI (regEn 0 en) address

    currentSamples :: Vec n (Signal dom a)
    currentSamples = map (regEn 0 en) $ zipWith (liftA2 (!!)) inputChunks addresses

    currentCoefficients :: Vec n (Signal dom a)
    currentCoefficients = map (regEn 0 en) $ zipWith func coeffs addresses
        where
        func coeffs idx = (coeffs !!) <$> idx

    multiplied :: Vec n (Signal dom a)
    multiplied = map (regEn 0 en) $ zipWith (*) currentCoefficients currentSamples

    outputStream :: Signal dom a
    outputStream = foldl func 0 multiplied
        where
        func accum x = regEn 0 en $ accum + x

    dumpIt :: Signal dom Bool
    dumpIt = last $ iterate (SNat @ (4 + n + m)) (regEn False en) stepChunk

    accum :: Signal dom a
    accum = regEn 0 en (mux dumpIt (pure 0) accum + outputStream)

