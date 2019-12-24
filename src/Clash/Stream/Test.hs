-- | Utilities for testing streams consisting of data and valid+ready signals
module Clash.Stream.Test (
        streamList,
        streamListHoldEnable,
        toPacketStream, 
        toPacketStreamList,
        fromPacketStream,
        fromPacketStreamList
    ) where

import Clash.Prelude
import qualified Prelude

-- | Create a stream from a list
streamList
    :: (HiddenClockResetEnable dom, NFDataX a)
    => [a]                             -- ^ List to stream
    -> [Bool]                          -- ^ Enable signal - useful if your design needs to be tolerant of the enable signal dropping out
    -> Signal dom Bool                 -- ^ Ready signal from downstream
    -> (Signal dom Bool, Signal dom a) -- ^ (Valid, Data)
streamList samples enables = unbundle . mealy step (samples, enables)
    where
    step :: ([a], [Bool]) -> Bool -> (([a], [Bool]), (Bool, a))
    step ([],       _)     _     = (([], []), (False, errorX "ran out of stream"))
    step (l@(x:xs), e:es)  ready = ((nextDats, es), (e, x))
        where
        nextDats 
            | e && ready = xs
            | otherwise  = l

-- | Create a stream from a list. Holds data and enable constant if downstream doesn't assert ready.
streamListHoldEnable
    :: (HiddenClockResetEnable dom, NFDataX a)
    => [a]                             -- ^ List to stream
    -> [Bool]                          -- ^ Enable signal - useful if your design needs to be tolerant of the enable signal dropping out
    -> Signal dom Bool                 -- ^ Ready signal from downstream
    -> (Signal dom Bool, Signal dom a) -- ^ (Valid, Data)
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

-- | Add an eof flag to the end of a packet
toPacketStream :: [a] -> [(a, Bool)]
toPacketStream []     = error "toStream: empty list"
toPacketStream [x]    = [(x, True)]
toPacketStream (x:xs) = (x, False) : toPacketStream xs

-- | Add an eof flag to a list of packets
toPacketStreamList :: [[a]] -> [(a, Bool)]
toPacketStreamList =  Prelude.concatMap toPacketStream 

-- | Extract a packet from a stream by reading until an eof
fromPacketStream :: [(a, Bool)] -> ([a], [(a, Bool)])
fromPacketStream [(x, False)] = ([x], [])
fromPacketStream ((x, last) : xs) 
    | last      = ([x], xs)
    | otherwise = let (ys, zs) = fromPacketStream xs in (x:ys, zs)

-- | Extract a list of packets from a stream
fromPacketStreamList :: [(a, Bool)] -> [[a]]
fromPacketStreamList [] = []
fromPacketStreamList xs = 
    let (first, rest) = fromPacketStream xs
    in  first : fromPacketStreamList rest

