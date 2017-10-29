module BCDSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HasClockReset)
import Test.Hspec
import Test.QuickCheck

import Data.Digits
import Clash.BCD

--BCD conversion testing
spec :: SpecWith ()
spec = describe "Binary to decimal conversion using the double dabble algorithm" $ 
    it "correctly converts binary numbers between 0 and 9999 to decimal" $ property $ 
        forAll (choose (0, 9999)) $ \(x :: Int) -> --Make sure that the input range is representable within the output type
            dropWhile (==0) (Clash.toList (toDec (fromIntegral x :: BitVector 16) :: Vec 4 BCDDigit)) == map fromIntegral (digits 10 x)

