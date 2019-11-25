{-# LANGUAGE RankNTypes #-}
module StreamPipelineSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable, NFDataX, System, fromList, (.&&.), sample)
import Test.Hspec
import Test.QuickCheck hiding ((.&&.), sample)

import Clash.Stream.Test
import Clash.Stream.Pipeline

spec = describe "Stream pipeline" $ do
    specify "test the test"    $ property $ prop dummy
    specify "forward pipeline" $ property $ prop forwardPipeline
    specify "skid buffer"      $ property $ prop skidBuffer
    specify "combined"         $ property $ prop combined

type StreamOperator a
    =  forall dom
    .  HiddenClockResetEnable dom
    => Signal dom Bool                                  -- ^ Input valid
    -> Signal dom a                                     -- ^ Data
    -> Signal dom Bool                                  -- ^ Downstream ready
    -> (Signal dom Bool, Signal dom a, Signal dom Bool) -- ^ (Output valid, output data, ready)

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

dummy = (,,)

prop :: StreamOperator Int -> InfiniteList Bool -> [Int] -> InfiniteList Bool -> Property
prop op (InfiniteList ens _) datas (InfiniteList readys _) = res === datas
    where
    res 
        = take (length datas) 
        $ map snd 
        $ filter fst 
        $ sample @System 
        $ bundle 
        $ system op (datas ++ repeat 0) (False : ens) readys

combined
    :: forall dom a
    .  (HiddenClockResetEnable dom, NFDataX a)
    => Signal dom Bool
    -> Signal dom a
    -> Signal dom Bool
    -> (Signal dom Bool, Signal dom a, Signal dom Bool)
combined vldIn datIn readyIn = (p3Vld, p3Dat, p1Rdy)
    where
    (p1Vld, p1Dat, p1Rdy) = forwardPipeline vldIn datIn p2Rdy
    (p2Vld, p2Dat, p2Rdy) = skidBuffer      p1Vld p1Dat p3Rdy
    (p3Vld, p3Dat, p3Rdy) = forwardPipeline p2Vld p2Dat readyIn

