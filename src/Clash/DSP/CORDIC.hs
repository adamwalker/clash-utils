{-| 
    Compute trigonometric functions using <https://en.wikipedia.org/wiki/CORDIC CODRIC>. See also the Wikibook: <https://en.wikibooks.org/wiki/Digital_Circuits/CORDIC>. 

    __FPGA proven__
-}
module Clash.DSP.CORDIC (
    arctans,
    kValue,
    Complex(..),
    realPart,
    imagPart,
    CordicState(..),
    DirFunc(..),
    dirMagPhase,
    dirRealImag,
    cordicStep,
    cordicSteps,
    cordicPipeline,
    toPolar,
    cordicExample
    ) where

import Clash.Prelude
import qualified Prelude as P

import Clash.DSP.Complex

{-| The gamma constants as described on the Wikipedia page -}
arctans :: [Double]
arctans = P.map func [0..]
    where
    func i = atan (2 ** (-i))

{-| The K constants as described on the Wikipedia page -}
kValue :: Int -> Double
kValue i = product $ P.take i $ P.map func [0..]
    where
    func i = 1 / sqrt (1 + 2 ** (-2 * i))

{-| The state between iterations of the CORDIC algorithm. It is parameterised by two types: the type of the vector and the type of the accumulating angle. -}
data CordicState a b = CordicState {
    cplx :: Complex a,
    arg  :: b
} deriving (Show, Generic, ShowX, NFDataX)

type DirFunc a b = CordicState a b -> Bool

dirMagPhase :: (Ord a, Num a) => DirFunc a b
dirMagPhase (CordicState (_ :+ y) _) = y < 0

dirRealImag :: (Ord b, Num b) => DirFunc a b
dirRealImag (CordicState _ a)        = a > 0

{-| Perform one step of the CORDIC algorithm. Can be used to calculate sine and cosine as well as calculate the magnitude and phase of a complex number. See the tests to see how this is done. This pure function can be used iteratively by feeding the output back into the input, pipelined by instantiating it several times with registers in between, or combinationally. `cordicSteps` may be useful for this. -}
cordicStep 
    :: (Num a, Bits a, Num b, KnownNat n) 
    => DirFunc a b     -- ^ Function that determines the direction of rotation. See the tests for an example.
    -> Index n         -- ^ Iteration index of this step
    -> b               -- ^ Arctan for this index
    -> CordicState a b -- ^ Input state
    -> CordicState a b -- ^ Output state
cordicStep dir idx a state@(CordicState (x :+ y) arg) = CordicState (nextX :+ nextY) nextArg
    where
    addSub sel x y --TODO: the arctan added/subtracted can be fewer bits than the accumulator
        | sel       = x + y
        | otherwise = x - y

    nextX   = addSub (not sel) x (y `shiftR` fromIntegral idx)
    nextY   = addSub sel       y (x `shiftR` fromIntegral idx)

    nextArg = addSub (not sel) arg a
    sel     = dir state

{-| Perform n iterations of the CORDIC algorithm -}
cordicSteps
    :: (Num a, Bits a, Num b, KnownNat n, KnownNat m) 
    => DirFunc a b     -- ^ Function that determines the direction of rotation
    -> Index m         -- ^ Iteration index of these steps
    -> Vec n b         -- ^ Vector of arctan values
    -> CordicState a b -- ^ Input state
    -> CordicState a b -- ^ Output state
cordicSteps dir start = flip (ifoldl cordicStep') 
    where 
    cordicStep' accum index con = cordicStep dir (start + resize index) con accum

cordicPipeline
    :: forall dom a b m numStages numPerStage
    .  HiddenClockResetEnable dom
    => KnownNat numStages
    => (NFDataX a, Bits a, Num a)
    => (NFDataX b, Num b)
    => KnownNat numPerStage
    => KnownNat m
    => DirFunc a b
    -> Index m
    -> Vec numStages (Vec numPerStage b)
    -> Signal dom Bool
    -> Signal dom (CordicState a b)
    -> Signal dom (CordicState a b)
cordicPipeline dir start consts en input 
    = foldl (flip step) input (zip (iterateI (+ (snatToNum (SNat @numPerStage))) start) consts)
    where 

    step (idx, coeff) 
        = delayEn (errorX "Initial CORDIC pipe") en 
        . fmap (step' idx coeff)
        where
        step' 
            :: Index m 
            -> Vec numPerStage b
            -> CordicState a b 
            -> CordicState a b
        step' = cordicSteps dir

toPolar 
    :: forall dom numStages numPerStage a b
    .  HiddenClockResetEnable dom
    => KnownNat numStages
    => KnownNat numPerStage
    => (Ord a, Bits a, NFDataX a, Num a)
    => (Bits b, Bounded b, NFDataX b, Num b)
    => Vec (numStages + 1) (Vec numPerStage b)
    -> Signal dom Bool
    -> Signal dom (Complex a)
    -> Signal dom (a, b)
toPolar consts en input = res
    where

    (flip, swizzled) = unbundle $ delayEn undefined en $ swizzle <$> input
        where
        swizzle c@(x :+ _)
            | x < 0     = (True,  fmap negate c)
            | otherwise = (False, c)

    preFlip 
        = cordicPipeline dirMagPhase (0 :: Index ((numStages + 1) * numPerStage)) consts en 
        $ CordicState <$> swizzled <*> pure 0

    flipD = last $ generate (SNat @(numStages + 1)) (delayEn (errorX "initial toPolar delay") en) flip

    res = func <$> flipD <*> preFlip
        where
        func False (CordicState (x :+ y) arg) = (x, arg)
        func True  (CordicState (x :+ y) arg) = (x, arg `xor` minBound)

{-| An example synthesizeable CORDIC implementation. Finds the magnitude and phase of a complex number. Consists of an 8 deep pipeline. Each pipeline stages performs two CORDIC iterations for a total of 16 iterations. Processes one input per cycle. Latency is 8 cycles. -}
cordicExample 
    :: HiddenClockResetEnable dom
    => Vec 16 (SFixed 2 16)                                  -- ^ Vector or arctans. Needs to be supplied as an argument to work around GHC's annoying "stage restriction".
    -> Signal dom (SFixed 16 16)                             -- ^ Real part
    -> Signal dom (SFixed 16 16)                             -- ^ Imaginary part
    -> Signal dom (CordicState (SFixed 16 16) (SFixed 2 16)) -- ^ Result. Real part of `cplx` is magnitude. `arg` contains argument.
cordicExample consts' x y 
    = fmap (step 14 $ consts !! 7)
    $ delay (errorX "initial cordicExample")
    $ fmap (step 12 $ consts !! 6)
    $ delay (errorX "initial cordicExample")
    $ fmap (step 10 $ consts !! 5)
    $ delay (errorX "initial cordicExample")
    $ fmap (step 8  $ consts !! 4)
    $ delay (errorX "initial cordicExample")
    $ fmap (step 6  $ consts !! 3)
    $ delay (errorX "initial cordicExample")
    $ fmap (step 4  $ consts !! 2)
    $ delay (errorX "initial cordicExample")
    $ fmap (step 2  $ consts !! 1)
    $ delay (errorX "initial cordicExample")
    $ fmap (step 0  $ consts !! 0)
    $ CordicState <$> cplx <*> pure (0 :: SFixed 2 16)
    where 

    step :: Index 16 -> Vec 2 (SFixed 2 16) -> CordicState (SFixed 16 16) (SFixed 2 16) -> CordicState (SFixed 16 16) (SFixed 2 16)
    step = cordicSteps (\(CordicState (_ :+ y) _) -> y < 0)

    cplx = liftA2 (:+) x y

    consts :: Vec 8 (Vec 2 (SFixed 2 16))
    consts = unconcatI consts'

