{-# LANGUAGE ScopedTypeVariables #-}
module CLaSH.CORDIC where

import CLaSH.Prelude
import qualified Prelude as P

tangents :: [Double]
tangents = P.map func [0..]
    where
    func i = atan (2 ** (-i))

kValue :: Int -> Double
kValue i = product $ P.take i $ P.map func [0..]
    where
    func i = 1 / sqrt (1 + 2 ** (-2 * i))

data Complex a = a :+ a deriving (Show)

data CordicState a b = CordicState {
    cplx :: Complex a,
    arg  :: b
} deriving (Show)

cordicStep 
    :: (Ord a, Num a, Bits a, Num b, Ord b, KnownNat n) 
    => Index n 
    -> b 
    -> CordicState a b 
    -> CordicState a b
cordicStep idx a (CordicState (x :+ y) arg) = CordicState (nextX :+ nextY) nextArg
    where
    addSub sel x y 
        | sel       = x + y
        | otherwise = x - y

    nextX   = addSub (not sel) x (y `shiftR` fromIntegral idx)
    nextY   = addSub sel       y (x `shiftR` fromIntegral idx)

    nextArg = addSub (not sel) arg a
    sel = y < 0

cordic 
    :: (Ord a, Num a, Bits a, Num b, Ord b, Fractional b, KnownNat n) 
    => Vec n b 
    -> CordicState a b 
    -> CordicState a b
cordic = flip (ifoldl cordicStep') 
    where 
    cordicStep' accum index con = cordicStep index con accum

