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
    specify "forward pipeline"                    $ property $ propStreamIdentity @Int forwardPipeline
    specify "backward pipeline"                   $ property $ propStreamIdentity @Int backwardPipeline
    specify "skid buffer with registered inputs"  $ property $ propStreamIdentity @Int skidBufferInReg
    specify "skid buffer with registered outputs" $ property $ propStreamIdentity @Int skidBufferOutReg

