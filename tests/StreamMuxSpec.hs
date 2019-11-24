{-# LANGUAGE RankNTypes #-}
module StreamMuxSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable, NFDataX, System, fromList, (.&&.), sample, toList, errorX)
import Test.Hspec
import Test.QuickCheck hiding ((.&&.), sample)

import Clash.Stream.Mux

import Data.List (sort)
import Data.Maybe (isNothing)

spec = describe "Stream mux" $ do
    specify "test the test" $ property $ prop_test
    specify "stream mux"    $ property $ prop streamMux
    specify "stream mux"    $ property $ prop streamMuxBiased

toStream :: [a] -> [(a, Bool)]
toStream []     = error "toStream: empty list"
toStream [x]    = [(x, True)]
toStream (x:xs) = (x, False) : toStream xs

toStreamList :: [[a]] -> [(a, Bool)]
toStreamList =  concatMap toStream 

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

prop_test = forAll (listOf (listOf1 arbitrary)) $ \(lists :: [[Int]]) -> 
    fromStreamList (toStreamList lists) === lists

streamList 
    :: (HiddenClockResetEnable dom, NFDataX a)
    => [a]
    -> [Bool]
    -> Signal dom Bool
    -> (Signal dom Bool, Signal dom a)
streamList samples enables = unbundle . mealy step (samples, enables)
    where
    step :: ([a], [Bool]) -> Bool -> (([a], [Bool]), (Bool, a))
    step (l@(x:xs), es@(True:_)) False = ((l, es),  (True, x))
    step (l@(x:xs), (e:es))      False = ((l, es),  (e, x))
    step (l@(x:xs), (False:es))  True  = ((l, es),  (False, x))
    step (  (x:xs), (True:es))   True  = ((xs, es), (True, x))
    step (_,        _)           _     = (([], []), (False, errorX "ran out of stream"))

type MuxType a
    =  forall dom
    .  (HiddenClockResetEnable dom)
    => Signal dom Bool                    -- ^ Downstream ready
    -> Vec 4 (Signal dom (Bool, Bool, a)) -- ^ Incoming streams
    -> (
            Vec 4 (Signal dom Bool),      -- ^ Upstream readys
            Signal dom (Bool, Bool, a)    -- ^ Outgoing streams
    )

system :: forall dom a. HiddenClockResetEnable dom => MuxType Int -> Vec 4 [[Int]] -> [Bool] -> [Bool] -> Signal dom (Bool, Bool, Int)
system mux streams vldIns readyIns = bundle (vldOut .&&. readySig, eofOut, datOut)
    where

    readySig = fromList readyIns
    
    streams' :: Vec 4 (Signal dom Bool, Signal dom (Int, Bool))
    streams' =  Clash.zipWith (\x readys -> streamList (toStreamList x) vldIns readys) streams readys

    (vldOut, eofOut, datOut) = unbundle streamOut

    (readys, streamOut) = mux readySig $ Clash.map func streams'
        where
        func :: (Signal dom Bool, Signal dom (Int, Bool)) -> Signal dom (Bool, Bool, Int)
        func (vld, dat) = (,,) <$> vld <*> eof <*> dat'
            where
            (dat', eof) = unbundle dat

prop :: MuxType Int -> InfiniteList Bool -> InfiniteList Bool -> Property
prop mux (InfiniteList validIns _) (InfiniteList readyIns _) 
    = forAll (sequenceA (Clash.repeat (listOf (listOf1 arbitrary)))) $ \(lists :: Vec 4 [[Int]]) ->
    let 
        flatList :: [[Int]]
        flatList = concat $ toList lists
        result 
            = take (length flatList)
            $ fromStreamList 
            $ map snd 
            $ filter fst 
            $ map swizzle 
            $ drop 1
            $ sample @System 
            $ system mux lists readyIns readyIns
            where
            swizzle :: (Bool, Bool, Int) -> (Bool, (Int, Bool))
            swizzle (vld, eof, dat) = (vld, (dat, eof))
    in sort flatList === sort result

