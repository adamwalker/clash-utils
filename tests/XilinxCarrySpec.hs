module XilinxCarrySpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockReset)
import Test.Hspec
import Test.QuickCheck

import Clash.Xilinx.Carry

--Misc
spec = describe "Xilinx carry chain" $ do
    specify "adds" $ property prop_xilinxCarryChainAdder

prop_xilinxCarryChainAdder :: Bool -> BitVector 32 -> BitVector 32 -> Bool
prop_xilinxCarryChainAdder cIn x y = xilinxCarryAdder cIn x y == x + y + (if cIn then 1 else 0)

