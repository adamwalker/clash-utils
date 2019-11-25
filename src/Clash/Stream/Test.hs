module Clash.Stream.Test (
        streamList,
        streamListHoldEnable,
        toStream, 
        toStreamList,
        fromStream,
        fromStreamList
    ) where

import Clash.Prelude
import qualified Prelude

streamList
    :: (HiddenClockResetEnable dom, NFDataX a)
    => [a]
    -> [Bool]
    -> Signal dom Bool
    -> (Signal dom Bool, Signal dom a)
streamList samples enables = unbundle . mealy step (samples, enables)
    where
    step :: ([a], [Bool]) -> Bool -> (([a], [Bool]), (Bool, a))
    step ([],       _)     _     = (([], []), (False, errorX "ran out of stream"))
    step (l@(x:xs), e:es)  ready = ((nextDats, es), (e, x))
        where
        nextDats 
            | e && ready = xs
            | otherwise  = l

streamListHoldEnable
    :: (HiddenClockResetEnable dom, NFDataX a)
    => [a]
    -> [Bool]
    -> Signal dom Bool
    -> (Signal dom Bool, Signal dom a)
streamListHoldEnable samples enables = unbundle . mealy step (samples, enables)
    where
    step :: ([a], [Bool]) -> Bool -> (([a], [Bool]), (Bool, a))
    step ([],       _)           _     = (([], []), (False, errorX "ran out of stream"))
    step (l@(x:xs), ens@(e:es))  ready = ((nextDats, nextEns), (e, x))
        where
        nextDats 
            | e && ready = xs
            | otherwise  = l
        nextEns 
            | e && not ready = ens
            | otherwise      = es

toStream :: [a] -> [(a, Bool)]
toStream []     = error "toStream: empty list"
toStream [x]    = [(x, True)]
toStream (x:xs) = (x, False) : toStream xs

toStreamList :: [[a]] -> [(a, Bool)]
toStreamList =  Prelude.concatMap toStream 

fromStream :: [(a, Bool)] -> ([a], [(a, Bool)])
fromStream [(x, False)] = ([x], [])
fromStream ((x, last) : xs) 
    | last      = ([x], xs)
    | otherwise = let (ys, zs) = fromStream xs in (x:ys, zs)

fromStreamList :: [(a, Bool)] -> [[a]]
fromStreamList [] = []
fromStreamList xs = 
    let (first, rest) = fromStream xs
    in  first : fromStreamList rest

