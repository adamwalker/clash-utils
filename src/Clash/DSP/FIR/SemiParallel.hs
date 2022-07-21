{-| 
    Semi-parallel. These accept a sample every Nth sample instead of every cycle.
    By reusing the DSPs over multiple cycles, longer filters can be implemented with the same DSP resources.

    __FPGA proven__
-}
module Clash.DSP.FIR.SemiParallel (
        macUnit,
        integrateAndDump,
        semiParallelFIRSystolic,
        macUnitSymmetric,
        oddSymmAccum,
        evenSymmAccum,
        evenSymmAccum2,
        semiParallelFIRSystolicSymmetric,
        semiParallelFIRTransposed,
        semiParallelFIRTransposedBlockRam,
    ) where

import Clash.Prelude

import Clash.DSP.Complex
import Clash.DSP.MAC
import Clash.Counter

--Integrate and dump. If the dump input is high, the accumulator input to the addition is zeroed on the current cycle.
integrateAndDump
    :: (HiddenClockResetEnable dom)
    => (Num a, NFDataX a)
    => Signal dom Bool -- ^ Input valid
    -> Signal dom Bool -- ^ Reset accumulator to 0.
    -> Signal dom a    -- ^ Reset value
    -> Signal dom a    -- ^ Data in
    -> Signal dom a    -- ^ Integrated data out
integrateAndDump step reset resetVal sampleIn = sum
    where
    sum = delayEn (errorX "initial sum") step $ mux reset resetVal sum + sampleIn

shiftReg 
    :: forall dom n a
    .  (HiddenClockResetEnable dom)
    => (NFDataX a, Num a)
    => (KnownNat n, 1 <= n)
    => Signal dom Bool
    -> Signal dom a
    -> Signal dom (Vec n a)
shiftReg shift dat = sequenceA shifts
    where
    shifts :: Vec n (Signal dom a)
    shifts =  generateI (delayEn (errorX "initial shift reg") shift) dat

--Multiply and accumulate unit
--Keeps a shift register of samples, and a coefficient ROM
--Registers after reading the shift register and coefficient rom
macUnit
    :: forall n dom coeffType inputType outputType
    .  (HiddenClockResetEnable dom)
    => (KnownNat n, 1 <= n)
    => (NFDataX inputType, Num inputType)
    => (NFDataX outputType, Num outputType) 
    => (Num coeffType, NFDataX coeffType)
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
    sampleShiftReg =  shiftReg (step .&&. shiftSamples) sampleIn

    sampleToMul 
        = delayEn (errorX "initial sampleToMul") step 
        $ liftA2 (!!) sampleShiftReg idx
    coeffToMul  
        = delayEn (errorX "initial coeffToMul") step 
        $ (coeffs !!) <$> idx
    macD        
        = delayEn (errorX "initial macD") step 
        $ mac step coeffToMul sampleToMul cascadeIn

semiParallelFIRSystolic
    :: forall numStages macDelay coeffsPerStage coeffType inputType outputType dom
    .  HiddenClockResetEnable dom
    => (KnownNat coeffsPerStage, 1 <= coeffsPerStage)
    => KnownNat numStages
    => (NFDataX inputType, Num inputType) 
    => (NFDataX outputType, Num outputType)
    => (NFDataX coeffType, Num coeffType)
    => MAC dom coeffType inputType outputType
    -> SNat macDelay
    -> Vec numStages (Vec coeffsPerStage coeffType)        -- ^ Filter coefficients partitioned by stage
    -> Signal dom outputType                                     -- ^ Cascade
    -> Signal dom Bool                                           -- ^ Input valid
    -> Signal dom inputType                                      -- ^ Sample
    -> (Signal dom Bool, Signal dom outputType, Signal dom Bool) -- ^ (Output valid, output data, ready)
semiParallelFIRSystolic mac macDelay coeffs cascadeIn valid sampleIn = (validOut, dataOut, ready)
    where

    --Delay the cascade
    cascadeDelayed = last $ generate (macDelay `addSNat` (SNat @2)) (delayEn (errorX "initial cascadeDelayed") globalStep) cascadeIn

    --The chain of shift registers and MAC units
    sampleOut = foldl func (cascadeDelayed, sampleIn) (zip3 coeffs indices (init shifts))
        where
        func 
            :: (Signal dom outputType, Signal dom inputType)
            -> (Vec coeffsPerStage coeffType, Signal dom (Index coeffsPerStage), Signal dom Bool)
            -> (Signal dom outputType, Signal dom inputType)
        func (cascadeIn, sampleIn) (coeffs, idx, shift) = macUnit mac coeffs idx shift globalStep cascadeIn sampleIn

    --Calculate the address in the coefficient bank we are up to
    --This is progressively shifted down the systolic array each time we loop around the coefficient bank
    --An alternative design would be to calculate the current index locally at each stage
    address :: Signal dom (Index coeffsPerStage)
    address = wrappingCounter maxBound globalStep

    --We are ready to accept input when the first stage is on its last coefficient
    ready :: Signal dom Bool
    ready =  address .==. pure maxBound

    --The whole thing operates in lockstep
    --We can step when:
    --  * We are not ready (and therefore are still looping through the buffer of samples in the first stage)
    --  * We have valid data incoming, which we will accept if we are processing the last coefficient
    globalStep :: Signal dom Bool
    globalStep =  not <$> ready .||. valid

    --`shifts`, `indices` are the shift register chains of shift signals for the sample buffers, and coefficient indices
    --Alternatively, `shift` could be derived from the current sample index
    shifts :: Vec (numStages + 1) (Signal dom Bool)
    shifts =  iterateI (regEn False globalStep) ready

    indices :: Vec numStages (Signal dom (Index coeffsPerStage))
    indices =  iterateI (regEn 0 globalStep) address

    --The output is valid if:
    --  * The final stage (the integrator) would be shifting a new sample in
    --  * plus, the MAC unit delay
    --  * plus, 1 cycle for the mac unit stage
    --  * plus, 1 cycle for the integrator
    validOut :: Signal dom Bool
    validOut 
        --TODO: globalStep here is not good for timing
        =    globalStep 
        .&&. last (generate (macDelay `addSNat` (SNat @2)) (regEn False globalStep) (last shifts))

    dataOut :: Signal dom outputType
    dataOut =  integrateAndDump globalStep validOut 0 $ fst sampleOut

--Multiply and accumulate unit
--Keeps a shift register of samples, and a coefficient ROM
--Registers after reading the shift register and coefficient rom
macUnitSymmetric
    :: forall n dom coeffType inputType outputType
    .  (HiddenClockResetEnable dom)
    => (KnownNat n, 1 <= n)
    => (NFDataX inputType, Num inputType)
    => (NFDataX outputType, Num outputType) 
    => (Num coeffType, NFDataX coeffType)
    => MACPreAdd dom coeffType inputType outputType
    -> Vec n coeffType                               -- ^ Filter coefficients
    -> Signal dom (Index n)                          -- ^ Index to multiply
    -> Signal dom Bool                               -- ^ Shift
    -> Signal dom Bool                               -- ^ Step
    -> Signal dom outputType                         -- ^ Sample
    -> Signal dom inputType                          -- ^ MAC cascade in
    -> Signal dom inputType
    -> (Signal dom outputType, (Signal dom inputType, Signal dom inputType)) -- ^ (MAC'd sample out, delayed input sample out)
macUnitSymmetric mac coeffs idx shiftSamples step cascadeIn forwardSampleIn reverseSampleIn = (macD, (forwardSampleToMul, reverseSampleSaved))
    where

    --Keep the shift registers in the forward and reverse directions
    forwardShiftReg :: Signal dom (Vec n inputType)
    forwardShiftReg =  shiftReg (step .&&. shiftSamples) forwardSampleIn

    reverseShiftReg :: Signal dom (Vec n inputType)
    reverseShiftReg =  shiftReg (step .&&. shiftSamples) reverseSampleIn

    --Extract the samples to add and multiply
    forwardSampleToMul 
        = delayEn (errorX "initial forwardSampleToMul") step 
        $ liftA2 (!!) forwardShiftReg idx
    reverseSampleToMul 
        = delayEn (errorX "initial reverseSampleToMul") step 
        $ liftA2 (!!) (reverse <$> reverseShiftReg) idx

    --Save the reverse direction sample
    shiftSamplesD   
        = regEn False step 
        $ regEn False step shiftSamples
    reverseSampleSaved 
        = delayEn (errorX "initial reverseSampleSaved") shiftSamplesD reverseSampleToMul

    coeffToMul  
        = delayEn (errorX "initial coeffToMul") step 
        $ (coeffs !!) <$> idx
    macD        
        = delayEn (errorX "initial macD") step 
        $ mac step coeffToMul forwardSampleToMul reverseSampleToMul cascadeIn

type SymmAccum dom n inputType outputType
    =  Signal dom Bool
    -> Signal dom (Index n)
    -> Signal dom Bool
    -> Signal dom inputType
    -> Signal dom outputType
    -> (Signal dom inputType, Signal dom Bool, Signal dom outputType)

oddSymmAccum 
    :: forall dom n inputType outputType macDelay
    .  HiddenClockResetEnable dom
    => (Num inputType, NFDataX inputType)
    => (Num outputType, NFDataX outputType)
    => SNat macDelay
    -> (inputType -> outputType)
    -> SymmAccum dom n inputType outputType
oddSymmAccum macDelay convert step _ shift forwardSample cascadeIn = (dataSaved, validOut, dataOut)
    where
    dataSaved = delayEn (errorX "initial dataSaved") (shift .&&. step) forwardSample
    --TOOD: use of dataSaved is incorrect when the MAC delay is long, we need to buffer more samples
    dataOut =  integrateAndDump step validOut (convert <$> dataSaved) cascadeIn
    --The output is valid if:
    --  * The final stage (the integrator) would be shifting a new sample in
    --  * plus, the MAC unit delay
    --  * plus, 1 cycle for the mac unit stage
    --  * plus, 1 cycle for the integrator
    validOut :: Signal dom Bool
    validOut 
        --TODO: globalStep here is not good for timing
        =    step 
        .&&. last (generate (macDelay `addSNat` (SNat @2)) (regEn False step) shift)

evenSymmAccum 
    :: forall dom n inputType outputType macDelay
    .  HiddenClockResetEnable dom
    => (Num inputType, NFDataX inputType)
    => (Num outputType, NFDataX outputType)
    => SNat macDelay
    -> (inputType -> inputType -> outputType)
    -> SymmAccum dom n inputType outputType
evenSymmAccum macDelay add step _ shift forwardSample cascadeIn = (data1, validOut, dataOut)
    where
    data0 = delayEn (errorX "initial data0") (shift .&&. step) forwardSample
    data1 = delayEn (errorX "initial data1") (shift .&&. step) data0
    --TOOD: use of dataSaved is incorrect when the MAC delay is long, we need to buffer more samples
    dataOut =  integrateAndDump step validOut (add <$> data0 <*> data1) cascadeIn
    --The output is valid if:
    --  * The final stage (the integrator) would be shifting a new sample in
    --  * plus, the MAC unit delay
    --  * plus, 1 cycle for the mac unit stage
    --  * plus, 1 cycle for the integrator
    validOut :: Signal dom Bool
    validOut 
        --TODO: globalStep here is not good for timing
        =    step 
        .&&. last (generate (macDelay `addSNat` (SNat @2)) (regEn False step) shift)

evenSymmAccum2
    :: forall n dom macDelay coeffType inputType outputType
    .  (HiddenClockResetEnable dom)
    => (KnownNat n, 1 <= n)
    => (NFDataX inputType, Num inputType)
    => (NFDataX outputType, Num outputType) 
    => (Num coeffType, NFDataX coeffType)
    => SNat macDelay
    -> MACPreAdd dom coeffType inputType outputType
    -> Vec n coeffType                                                -- ^ Filter coefficients
    -> SymmAccum dom n inputType outputType
evenSymmAccum2 macDelay mac coeffs step idx shift forwardSampleIn cascadeIn = (reverseSampleSaved, validOut, dataOut)
    where

    --Keep the shift registers in the forward and reverse directions
    forwardShiftReg :: Signal dom (Vec n inputType)
    forwardShiftReg =  shiftReg (step .&&. shift) forwardSampleIn

    reverseSampleIn :: Signal dom inputType
    reverseSampleIn =  delayEn (errorX "initial reverseSampleIn") (step .&&. shift) forwardSampleToMul

    reverseShiftReg :: Signal dom (Vec n inputType)
    reverseShiftReg =  shiftReg (step .&&. shift) reverseSampleIn

    --Extract the samples to add and multiply
    forwardSampleToMul 
        = delayEn (errorX "initial forwardSampleToMul") step 
        $ liftA2 (!!) forwardShiftReg idx
    reverseSampleToMul 
        = delayEn (errorX "initial reverseSampleToMul") step 
        $ liftA2 (!!) (reverse <$> reverseShiftReg) idx

    --Save the reverse direction sample
    shiftSamplesD   
        = regEn False step 
        $ regEn False step shift
    reverseSampleSaved 
        = delayEn (errorX "initial reverseSampleSaved") shiftSamplesD reverseSampleToMul

    coeffToMul  
        = delayEn (errorX "initial coeffToMul") step 
        $ (coeffs !!) <$> idx
    macD        
        = delayEn (errorX "initial macD") step 
        $ mac step coeffToMul forwardSampleToMul reverseSampleToMul cascadeIn

    --TOOD: use of dataSaved is incorrect when the MAC delay is long, we need to buffer more samples
    dataOut =  integrateAndDump step validOut 0 macD
    --The output is valid if
    --  * The final stage (the integrator) would be shifting a new sample in
    --  * plus, the MAC unit delay
    --  * plus, 1 cycle for the mac unit stage
    --  * plus, 1 cycle for the integrator
    validOut :: Signal dom Bool
    validOut 
        --TODO: globalStep here is not good for timing
        =    step 
        .&&. last (generate (macDelay `addSNat` (SNat @3)) (regEn False step) shift)

semiParallelFIRSystolicSymmetric
    :: forall numStages macDelay coeffsPerStage coeffType inputType outputType dom
    .  HiddenClockResetEnable dom
    => (KnownNat coeffsPerStage, 1 <= coeffsPerStage)
    => KnownNat numStages
    => (NFDataX inputType, Num inputType) 
    => (NFDataX outputType, Num outputType)
    => (NFDataX coeffType, Num coeffType)
    => MACPreAdd dom coeffType inputType outputType
    -> SymmAccum dom coeffsPerStage inputType outputType
    -> SNat macDelay
    -> Vec numStages (Vec coeffsPerStage coeffType)        -- ^ Filter coefficients partitioned by stage
    -> Signal dom outputType
    -> Signal dom Bool                                           -- ^ Input valid
    -> Signal dom inputType                                      -- ^ Sample
    -> (Signal dom Bool, Signal dom outputType, Signal dom Bool) -- ^ (Output valid, output data, ready)
semiParallelFIRSystolicSymmetric mac symmAccum macDelay coeffs cascadeIn valid sampleIn = (dataValid, dataOut, ready)
    where

    --Delay the cascade
    cascadeDelayed = last $ generate (macDelay `addSNat` (SNat @2)) (delayEn (errorX "initial cascadeDelayed") globalStep) cascadeIn

    --The chain of shift registers and MAC units
    (_loopedBackSample, dataValid, dataOut) = foldr step (symmAccum globalStep (last indices) (last shifts)) (zip3 coeffs (init indices) (init shifts)) sampleIn cascadeDelayed
        where
        step (coeffs, index, shift) accum forwardSample cascadeIn = (reverseSample', sampleValid, sampleOut')
            where
            (reverseSample, sampleValid, sampleOut') 
                = accum forwardSample' sampleOut
            (sampleOut, (forwardSample', reverseSample')) 
                = macUnitSymmetric mac coeffs index shift globalStep cascadeIn forwardSample reverseSample

    --Calculate the address in the coefficient bank we are up to
    --This is progressively shifted down the systolic array each time we loop around the coefficient bank
    --An alternative design would be to calculate the current index locally at each stage
    address :: Signal dom (Index coeffsPerStage)
    address = wrappingCounter maxBound globalStep

    --We are ready to accept input when the first stage is on its last coefficient
    ready :: Signal dom Bool
    ready =  address .==. pure maxBound

    --The whole thing operates in lockstep
    --We can step when:
    --  * We are not ready (and therefore are still looping through the buffer of samples in the first stage)
    --  * We have valid data incoming, which we will accept if we are processing the last coefficient
    globalStep :: Signal dom Bool
    globalStep =  not <$> ready .||. valid

    --`shifts`, `indices` are the shift register chains of shift signals for the sample buffers, and coefficient indices
    --Alternatively, `shift` could be derived from the current sample index
    shifts :: Vec (numStages + 1) (Signal dom Bool)
    shifts =  iterateI (regEn False globalStep) ready

    indices :: Vec (numStages + 1) (Signal dom (Index coeffsPerStage))
    indices =  iterateI (regEn 0 globalStep) address

semiParallelFIRTransposed
    :: forall dom numStages coeffsPerStage coeffType inputType outputType
    .  HiddenClockResetEnable dom
    => (KnownNat numStages, KnownNat coeffsPerStage, 1 <= coeffsPerStage)
    => (NFDataX inputType, Num inputType)
    => (NFDataX outputType, Num outputType)
    => (NFDataX coeffType, Num coeffType)
    => MAC dom coeffType inputType outputType
    -> Vec numStages (Vec coeffsPerStage coeffType)
    -> Signal dom Bool
    -> Signal dom inputType
    -> (Signal dom Bool, Signal dom outputType, Signal dom Bool)
semiParallelFIRTransposed mac coeffs valid sampleIn = (validOut, dataOut, ready)
    where

    delayStage :: Signal dom inputType -> Signal dom inputType
    delayStage x = last $ iterate (SNat @(numStages + 1)) (regEn 0 ready) x

    delayLine :: Vec coeffsPerStage (Signal dom inputType)
    delayLine =  iterateI delayStage sampleIn

    stageCounter :: Signal dom (Index coeffsPerStage)
    stageCounter =  wrappingCounter 0 globalStep

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
    .  HiddenClockResetEnable dom
    => (KnownNat numStages, KnownNat coeffsPerStage, 1 <= coeffsPerStage)
    => (NFDataX inputType, Num inputType)
    => (NFDataX outputType, Num outputType)
    => NFDataX coeffType
    => MAC dom coeffType inputType outputType
    -> Vec numStages (Vec coeffsPerStage coeffType)
    -> Signal dom Bool
    -> Signal dom inputType
    -> (Signal dom Bool, Signal dom outputType, Signal dom Bool)
semiParallelFIRTransposedBlockRam mac coeffs valid sampleIn = (validOut, dataOut, ready)
    where

    stageCounter :: Signal dom (Index coeffsPerStage)
    stageCounter =  wrappingCounter maxBound globalStep 

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
            | ptr < snatToNum (SNat @numStages)
                = ptr + snatToNum (SNat @((coeffsPerStage - 1) * numStages))
            | otherwise 
                = ptr - snatToNum (SNat @numStages)

    --Clash's BlockRam doesn't support a read enable!
    --So fake it with an async ram followed by a register
    --TODO: check this synthesizes to a block ram
    sampleRamOut :: Signal dom inputType
    sampleRamOut 
        = regEn 0 globalStep $ asyncRam 
            (SNat @(numStages * coeffsPerStage)) 
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

