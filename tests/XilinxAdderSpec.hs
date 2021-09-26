module XilinxAdderSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable, extend, shiftL)
import Test.Hspec
import Test.QuickCheck

import Clash.Xilinx.Adder

--Misc
spec = describe "Xilinx carry chain" $ do
    specify "4:2 compressor" $ property prop_compressor
    specify "ternary adder"  $ property prop_ternaryAdder

prop_compressor :: Bool -> Vec 4 (BitVector 8) -> Property
prop_compressor cIn xs = expect === result
    where

    (sums, carrys) = compressor cIn $ Clash.map unpack xs
    result         = extend (pack sums) + shiftL (extend (pack carrys)) 1

    expect :: BitVector 10
    expect =  sum (Clash.map extend xs) + extend (pack cIn)

prop_ternaryAdder :: Bool -> Bool -> Vec 3 (BitVector 8) -> Property
prop_ternaryAdder cIn cIn2 xs = expect === (result + shiftL (extend (pack cOut)) 8)
    where

    (result, cOut) = ternaryAdder cIn cIn2 xs

    expect :: BitVector 9
    expect =  sum (Clash.map extend xs) + extend (pack cIn) + extend (pack cIn2)
