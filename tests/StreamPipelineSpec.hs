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
    specify "forward pipeline" $ property $ propStreamIdentity forwardPipeline
    specify "skid buffer"      $ property $ propStreamIdentity skidBuffer
    specify "combined"         $ property $ propStreamIdentity combined

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

