module GrayCodeSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable)
import Test.Hspec
import Test.QuickCheck

import Data.Digits
import Clash.GrayCode

--Gray code
spec = describe "Gray code" $ do
    specify "binaryToGray . grayToBinary == id" $ property prop_grayCode
    specify "grayToBinary . binaryToGray == id" $ property prop_grayCode2

prop_grayCode :: BitVector 32 -> Bool
prop_grayCode x = x == binaryToGray (grayToBinary x)

prop_grayCode2 :: BitVector 32 -> Bool
prop_grayCode2 x = x == grayToBinary (binaryToGray x)

