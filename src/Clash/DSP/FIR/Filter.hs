{-| FIR filters: see <http://adamwalker.github.io/Filter-Design-in-Clash/>.

    These are based on designs in the Xilinx document <http://www-inst.eecs.berkeley.edu/~cs150/fa13/resources/dsp.pdf DSP: Designing for Optimal Results>

    __FPGA proven__
-}
module Clash.DSP.FIR.Filter (
        fir,
        firTransposed,
        firSystolic,
        firSymmetric,
        firSystolicSymmetric,
        firSystolicSymmetricOdd,
        firSystolicHalfBand
    ) where

import Clash.Prelude

import Clash.DSP.Complex
import Clash.DSP.MAC

{- | Direct form FIR filter -}
fir 
    :: (HiddenClockResetEnable dom, KnownNat n, NFDataX inputType, Num inputType) 
    => (Signal dom coeffType  -> Signal dom inputType  -> Signal dom outputType) -- ^ Function to do the multiplication
    -> (Signal dom outputType -> Signal dom outputType -> Signal dom outputType) -- ^ Function to do the accumulation
    -> Vec (n + 1) coeffType                                                     -- ^ Coefficients
    -> Signal dom Bool                                                           -- ^ Input enable
    -> Signal dom inputType                                                      -- ^ Input samples
    -> Signal dom outputType                                                     -- ^ Output samples
fir mul add coeffs en x = dotp (map pure coeffs) (iterateI (regEn 0 en) x)
    where
    dotp as bs = fold add $ zipWith mul as bs

{- | Transposed FIR filter -}
firTransposed 
    :: (HiddenClockResetEnable dom, KnownNat n, NFDataX outputType, Num outputType) 
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
    :: (HiddenClockResetEnable dom, KnownNat n, NFDataX outputType, Num outputType, Num inputType, NFDataX inputType) 
    => MAC dom coeffType inputType outputType -- ^ Function to do the multiply and accumulate
    -> Vec (n + 1) coeffType                  -- ^ Coefficients
    -> Signal dom Bool                        -- ^ Input enable
    -> Signal dom inputType                   -- ^ Input samples
    -> Signal dom outputType                  -- ^ Output samples
firSystolic mac coeffs en x = foldl func 0 $ zip (map pure coeffs) $ iterateI (regEn 0 en . regEn 0 en) x
    where
    func accum (coeff, input) = regEn 0 en $ mac en coeff input accum

--TODO: symmetric odd and Symmetric half band
{- | Symmetric FIR filter -}
firSymmetric
    :: (HiddenClockResetEnable dom, KnownNat n, Num inputType, NFDataX inputType, Num outputType, NFDataX outputType) 
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
    :: (HiddenClockResetEnable dom, KnownNat n, Num outputType, NFDataX outputType, Num inputType, NFDataX inputType) 
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
    :: (HiddenClockResetEnable dom, KnownNat n, Num outputType, NFDataX outputType, Num inputType, NFDataX inputType) 
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
    :: (HiddenClockResetEnable dom, KnownNat n, Num inputType, Num outputType, NFDataX inputType, NFDataX outputType) 
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

