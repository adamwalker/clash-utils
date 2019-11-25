{-# LANGUAGE RankNTypes #-}
module StreamMuxSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable, NFDataX, System, fromList, (.&&.), sample, toList, errorX)
import Test.Hspec
import Test.QuickCheck hiding ((.&&.), sample)

import Clash.Stream.Test
import Clash.Stream.Mux

import Data.List (sort)
import Data.Maybe (isNothing)

spec = describe "Stream mux" $ do
    specify "stream mux" $ property $ prop streamMux
    specify "stream mux" $ property $ prop streamMuxBiased

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

