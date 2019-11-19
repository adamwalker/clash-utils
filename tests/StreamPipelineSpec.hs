{-# LANGUAGE RankNTypes #-}
module StreamPipelineSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable, NFDataX, System, fromList, (.&&.), sample)
import Test.Hspec
import Test.QuickCheck hiding ((.&&.), sample)

import Clash.Stream.Pipeline

spec = describe "Stream pipeline" $ do
    specify "test the test" $ property $ prop dummy
    specify "pipeline"      $ property $ prop pipeline
    specify "skid buffer"   $ property $ prop skidBuffer

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

