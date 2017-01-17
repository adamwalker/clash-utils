{-# LANGUAGE ScopedTypeVariables #-}

{-| Compute trigonometric functions using <https://en.wikipedia.org/wiki/CORDIC CODRIC>. See also the Wikibook: <https://en.wikibooks.org/wiki/Digital_Circuits/CORDIC>. -}
module CLaSH.CORDIC (
    arctans,
    kValue,
    Complex(..),
    realPart,
    imagPart,
    CordicState(..),
    cordicStep,
    cordicSteps
    ) where

import CLaSH.Prelude
import qualified Prelude as P

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

{-| I defined my own complex type so that I can write a Num instance without the RealFloat constraint. TODO: think about whether this is really a good idea. -}
data Complex a = a :+ a deriving (Show)

realPart :: Complex a -> a
realPart (x :+ _) = x

imagPart :: Complex a -> a
imagPart (_ :+ y) = y

{-| The state between iterations of the CORDIC algorithm. It is parameterised by two types: the type of the vector and the type of the accumulating angle. -}
data CordicState a b = CordicState {
    cplx :: Complex a,
    arg  :: b
} deriving (Show)

{-| Perform one step of the CORDIC algorithm. Can be used to calculate sine and cosine as well as calculate the magnitude and phase of a complex number. See the tests to see how this is done. This pure function can be used iteratively by feeding the output back into the input, pipelined by instantiating it several times with registers in between, or combinationally. `cordicSteps` may be useful for this. -}
cordicStep 
    :: (Ord a, Num a, Bits a, Num b, Ord b, KnownNat n) 
    => (CordicState a b -> Bool) -- ^ Function that determines the direction of rotation. See the tests for an example.
    -> Index n                   -- ^ Iteration index of this step
    -> b                         -- ^ Arctan for this index
    -> CordicState a b           -- ^ Input state
    -> CordicState a b           -- ^ Output state
cordicStep dir idx a state@(CordicState (x :+ y) arg) = CordicState (nextX :+ nextY) nextArg
    where
    addSub sel x y 
        | sel       = x + y
        | otherwise = x - y

    nextX   = addSub (not sel) x (y `shiftR` fromIntegral idx)
    nextY   = addSub sel       y (x `shiftR` fromIntegral idx)

    nextArg = addSub (not sel) arg a
    sel     = dir state

{-| Perform n iterations of the CORDIC algorithm -}
cordicSteps
    :: (Ord a, Num a, Bits a, Num b, Ord b, Fractional b, KnownNat n) 
    => (CordicState a b -> Bool) -- ^ Function that determines the direction of rotation
    -> Vec n b                   -- ^ Vector of arctan values
    -> CordicState a b           -- ^ Input state
    -> CordicState a b           -- ^ Output state
cordicSteps dir = flip (ifoldl cordicStep') 
    where 
    cordicStep' accum index con = cordicStep dir index con accum

