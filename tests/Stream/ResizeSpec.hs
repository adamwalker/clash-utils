{-# LANGUAGE RankNTypes #-}
module Stream.ResizeSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable, NFDataX, System, fromList, (.&&.), sample)
import Test.Hspec
import Test.QuickCheck hiding ((.&&.), sample)

import Clash.Stream.Test
import Clash.Stream.Resize

spec = describe "Stream resize" $ do
    specify "narrow -> widen is identity" $ property $ propPacketsIdentity narrowThenWiden

narrowThenWiden 
    :: forall dom
    .  HiddenClockResetEnable dom
    => Signal dom Bool                                                  -- ^ Input valid
    -> Signal dom (Vec 4 Int, Bool)                                     -- ^ Data
    -> Signal dom Bool                                                  -- ^ Downstream ready
    -> (Signal dom Bool, Signal dom (Vec 4 Int, Bool), Signal dom Bool) -- ^ (Output valid, output data, ready)
narrowThenWiden vld dat rdy = (vld'', bundle (dat'', eof''), readyIn)
    where
    (dat', eof')               = unbundle dat
    (narrowedStream, readyIn)  = narrowStream (bundle (vld, eof', dat')) readyNarrowed
    (streamOut, readyNarrowed) = widenStream narrowedStream rdy
    (vld'', eof'', dat'')      = unbundle streamOut
