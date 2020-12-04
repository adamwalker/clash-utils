{-| FIR filters: see <http://adamwalker.github.io/Filter-Design-in-Clash/>.

    These are based on designs in the Xilinx document <http://www-inst.eecs.berkeley.edu/~cs150/fa13/resources/dsp.pdf DSP: Designing for Optimal Results>

    __FPGA proven__
-}
module Clash.DSP.FIRFilter (
    fir,
    firTransposed,
    firSystolic,
    firSymmetric,
    firSystolicSymmetric,
    firSystolicSymmetricOdd,
    firSystolicHalfBand,
    macUnit,
    integrate,
    semiParallelFIRSystolic,
    semiParallelFIRTransposed,
    semiParallelFIRTransposedBlockRam,
    semiParallelFIR
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

macUnit
    :: forall n dom coeffType inputType outputType
    .  (HiddenClockResetEnable dom, KnownNat n, NFDataX inputType, Num inputType, NFDataX outputType, Num outputType, Num coeffType, NFDataX coeffType)
    => MAC dom coeffType inputType outputType
    -> Vec n coeffType                               -- ^ Filter coefficients
    -> Signal dom (Index n)                          -- ^ Index to multiply
    -> Signal dom Bool                               -- ^ Shift
    -> Signal dom Bool                               -- ^ Step
    -> Signal dom outputType                         -- ^ Sample
    -> Signal dom inputType                          -- ^ MAC cascade in
    -> (Signal dom outputType, Signal dom inputType) -- ^ (MAC'd sample out, delayed input sample out)
macUnit mac coeffs idx shiftSamples step cascadeIn sampleIn = (macD, sampleToMul)
    where

    sampleShiftReg :: Signal dom (Vec n inputType)
    sampleShiftReg =  regEn (repeat 0) (step .&&. shiftSamples) $ (+>>) <$> sampleIn <*> sampleShiftReg

    sampleToMul = regEn 0 step $ (!!) <$> sampleShiftReg <*> idx
    coeffToMul  = regEn 0 step $ (coeffs !!) <$> idx
    macD        = regEn 0 step $ mac step coeffToMul sampleToMul cascadeIn

integrate
    :: (HiddenClockResetEnable dom, Num a, NFDataX a)
    => Signal dom Bool -- ^ Input valid
    -> Signal dom Bool -- ^ Reset accumulator to 0. Will apply to new data on _this_ cycle.
    -> Signal dom a    -- ^ Data in
    -> Signal dom a    -- ^ Integrated data out
integrate step reset sampleIn = sum
    where
    sum = regEn 0 step $ mux reset 0 sum + sampleIn

semiParallelFIRSystolic
    :: forall numStages coeffsPerStage coeffType inputType outputType dom
    .  (HiddenClockResetEnable dom, KnownNat coeffsPerStage, KnownNat numStages, NFDataX inputType, NFDataX outputType, Num inputType, Num outputType, Num coeffType, NFDataX coeffType)
    => MAC dom coeffType inputType outputType
    -> Vec (numStages + 1) (Vec coeffsPerStage coeffType)        -- ^ Filter coefficients partitioned by stage
    -> Signal dom Bool                                           -- ^ Input valid
    -> Signal dom inputType                                      -- ^ Sample
    -> (Signal dom Bool, Signal dom outputType, Signal dom Bool) -- ^ (Output valid, output data, ready)
semiParallelFIRSystolic mac coeffs valid sampleIn = (validOut, dataOut, ready)
    where
    sampleOut = foldl func (0, sampleIn) (zip3 coeffs indices shifts)
        where
        func 
            :: (Signal dom outputType, Signal dom inputType)
            -> (Vec coeffsPerStage coeffType, Signal dom (Index coeffsPerStage), Signal dom Bool)
            -> (Signal dom outputType, Signal dom inputType)
        func (cascadeIn, sampleIn) (coeffs, idx, shift) = macUnit mac coeffs idx shift globalStep cascadeIn sampleIn

    globalStep :: Signal dom Bool
    globalStep =  address ./=. pure maxBound .||. valid

    shifts :: Vec (numStages + 1) (Signal dom Bool)
    shifts =  iterateI (regEn False globalStep) $ address .==. pure maxBound

    address :: Signal dom (Index coeffsPerStage)
    address = regEn maxBound globalStep (wrappingInc <$> address)
        where
        wrappingInc x
            | x == maxBound = 0
            | otherwise     = x + 1

    indices :: Vec (numStages + 1) (Signal dom (Index coeffsPerStage))
    indices =  iterateI (regEn 0 globalStep) address

    validOut :: Signal dom Bool
    validOut =  globalStep .&&. (regEn False globalStep $ regEn False globalStep $ last indices .==. 0)

    dataOut :: Signal dom outputType
    dataOut =  integrate globalStep validOut $ fst sampleOut

    ready :: Signal dom Bool
    ready =  address .==. pure maxBound

semiParallelFIRTransposed
    :: forall dom numStages coeffsPerStage coeffType inputType outputType
    .  (HiddenClockResetEnable dom, KnownNat numStages, KnownNat coeffsPerStage, 1 <= coeffsPerStage, NFDataX inputType, Num inputType, NFDataX outputType, Num outputType, Num coeffType, NFDataX coeffType)
    => MAC dom coeffType inputType outputType
    -> Vec numStages (Vec coeffsPerStage coeffType)
    -> Signal dom Bool
    -> Signal dom inputType
    -> (Signal dom Bool, Signal dom outputType, Signal dom Bool)
semiParallelFIRTransposed mac coeffs valid sampleIn = (validOut, dataOut, ready)
    where

    delayStage :: Signal dom inputType -> Signal dom inputType
    delayStage x = last $ iterate (SNat @ (numStages + 1)) (regEn 0 ready) x

    delayLine :: Vec coeffsPerStage (Signal dom inputType)
    delayLine =  iterateI delayStage sampleIn

    stageCounter :: Signal dom (Index coeffsPerStage)
    stageCounter =  regEn 0 globalStep $ wrappingInc <$> stageCounter
        where
        wrappingInc x
            | x == maxBound = 0
            | otherwise     = x + 1

    globalStep :: Signal dom Bool
    globalStep =  valid .||. stageCounter ./=. 0

    ready :: Signal dom Bool
    ready =  stageCounter .==. pure maxBound 

    newCascadeIn :: Signal dom Bool
    newCascadeIn =  stageCounter .==. 0

    delayedSampleIn :: Signal dom inputType
    delayedSampleIn =  liftA2 (!!) (sequenceA delayLine) stageCounter

    dataOut :: Signal dom outputType
    dataOut =  foldl accumFunc (pure 0) coeffs
        where
        accumFunc :: Signal dom outputType -> Vec coeffsPerStage coeffType -> Signal dom outputType
        accumFunc cascadeIn coeffs = accum
            where
            cascadeIn' = mux newCascadeIn cascadeIn accum
            coeff      = fmap (coeffs !!) stageCounter
            accum      = regEn 0 globalStep $ mac globalStep coeff delayedSampleIn cascadeIn' 

    validOut :: Signal dom Bool
    validOut =  register False (stageCounter .==. pure maxBound)

semiParallelFIRTransposedBlockRam
    :: forall dom numStages coeffsPerStage coeffType inputType outputType
    .  (HiddenClockResetEnable dom, KnownNat numStages, KnownNat coeffsPerStage, 1 <= coeffsPerStage, NFDataX inputType, Num inputType, NFDataX outputType, Num outputType, NFDataX coeffType)
    => MAC dom coeffType inputType outputType
    -> Vec numStages (Vec coeffsPerStage coeffType)
    -> Signal dom Bool
    -> Signal dom inputType
    -> (Signal dom Bool, Signal dom outputType, Signal dom Bool)
semiParallelFIRTransposedBlockRam mac coeffs valid sampleIn = (validOut, dataOut, ready)
    where

    stageCounter :: Signal dom (Index coeffsPerStage)
    stageCounter =  regEn maxBound globalStep $ wrappingInc <$> stageCounter
        where
        wrappingInc x
            | x == maxBound = 0
            | otherwise     = x + 1

    writePtr :: Signal dom (Index (numStages * coeffsPerStage))
    writePtr =  regEn 0 (ready .&&. valid) $ step <$> writePtr
        where
        step x 
            | x == maxBound = 0
            | otherwise     = x + 1

    readPtr :: Signal dom (Index (numStages * coeffsPerStage))
    readPtr =  regEn 0 globalStep $ step <$> readPtr <*> ready <*> writePtr
        where
        step _   True  writePtr = writePtr
        step ptr _     _
            | ptr < snatToNum (SNat @ numStages)
                = ptr + snatToNum (SNat @ ((coeffsPerStage - 1) * numStages))
            | otherwise 
                = ptr - snatToNum (SNat @ numStages)

    --Clash's BlockRam doesn't support a read enable!
    --So fake it with an async ram followed by a register
    --TODO: check this synthesizes to a block ram
    sampleRamOut :: Signal dom inputType
    sampleRamOut 
        = regEn 0 globalStep $ asyncRam 
            (SNat @ (numStages * coeffsPerStage)) 
            readPtr 
            (mux (ready .&&. valid) (Just <$> bundle (writePtr, sampleIn)) (pure Nothing))

    globalStep :: Signal dom Bool
    globalStep =  valid .||. stageCounter ./=. pure maxBound

    ready :: Signal dom Bool
    ready =  stageCounter .==. pure maxBound 

    newCascadeIn :: Signal dom Bool
    newCascadeIn =  regEn False globalStep $ stageCounter .==. 0

    dataOut :: Signal dom outputType
    dataOut =  foldl accumFunc (pure 0) coeffs
        where
        accumFunc :: Signal dom outputType -> Vec coeffsPerStage coeffType -> Signal dom outputType
        accumFunc cascadeIn coeffs = accum
            where
            cascadeIn' = mux newCascadeIn cascadeIn accum
            coeff      = regEn (errorX "initial coeff") globalStep $ fmap (coeffs !!) stageCounter
            accum      = regEn 0 globalStep $ mac globalStep coeff sampleRamOut cascadeIn' 

    validOut :: Signal dom Bool
    validOut =  register False (stageCounter .==. 0)

semiParallelFIR 
    :: forall dom a n m n' m'. (HiddenClockResetEnable dom, Num a, KnownNat n, KnownNat m, n ~ (n' + 1), m ~ (m' + 1), NFDataX a)
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

