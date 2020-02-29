{-# LANGUAGE RankNTypes #-}
-- | Utilities for testing streams consisting of data and valid+ready signals
module Clash.Stream.Test (
        streamList,
        streamListHoldEnable,
        toPacketStream, 
        toPacketStreamList,
        fromPacketStream,
        fromPacketStreamList,
        StreamOperator,
        propStreamIdentity,
        propPacketsIdentity
    ) where

import Clash.Prelude (NFDataX, System, Signal, HiddenClockResetEnable, fromList, bundle, unbundle, mealy, errorX, sample, (.&&.), Default(..))
import Prelude
import Test.QuickCheck hiding (sample, (.&&.))

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
toPacketStreamList = concatMap toPacketStream 

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

-- | Type for operations on streams
type StreamOperator a
    =  forall dom
    .  HiddenClockResetEnable dom
    => Signal dom Bool                                  -- ^ Input valid
    -> Signal dom a                                     -- ^ Data
    -> Signal dom Bool                                  -- ^ Downstream ready
    -> (Signal dom Bool, Signal dom a, Signal dom Bool) -- ^ (Output valid, output data, ready)

-- | Helper property for stream operations that should be the identity
propStreamIdentity :: StreamOperator Int -> InfiniteList Bool -> [Int] -> InfiniteList Bool -> Property
propStreamIdentity op (InfiniteList ens _) datas (InfiniteList readys _) = res === datas
    where
    res 
        = take (length datas) 
        $ map snd 
        $ filter fst 
        $ sample @System 
        $ bundle 
        $ system op (datas ++ repeat 0) (False : ens) readys
    system 
        :: forall dom a
        .  (HiddenClockResetEnable dom, NFDataX a, Num a)
        => StreamOperator a
        -> [a]
        -> [Bool]
        -> [Bool]
        -> (Signal dom Bool, Signal dom a)
    system streamOp dat ens readys = (vld .&&. backPressure, out)
        where
        backPressure         = fromList readys
        (valids, dataStream) = streamList dat ens ready
        (vld, out, ready)    = streamOp valids dataStream backPressure

propPacketsIdentity
    :: (NFDataX a, Default a, Eq a, Show a, Arbitrary a) 
    => StreamOperator (a, Bool) 
    -> InfiniteList Bool 
    -> InfiniteList Bool 
    -> Property
propPacketsIdentity op (InfiniteList ens _) (InfiniteList readys _) = 
    forAll (listOf (listOf1 arbitrary)) $ \datas -> 
    let 
        res 
            = take (length datas) 
            $ fromPacketStreamList
            $ map snd 
            $ filter fst 
            $ sample @System 
            $ bundle 
            $ system op datas (False : ens) readys
    in
        res === datas
    where
    system 
        :: forall dom a
        .  (HiddenClockResetEnable dom, NFDataX a, Default a)
        => StreamOperator (a, Bool)
        -> [[a]]
        -> [Bool]
        -> [Bool]
        -> (Signal dom Bool, Signal dom (a, Bool))
    system streamOp packets ens readys = (vld .&&. readySig, out)
        where
        readySig             = fromList readys
        (valids, dataStream) = streamList (toPacketStreamList packets ++ repeat (def, False)) ens ready
        (vld, out, ready)    = streamOp valids dataStream readySig

