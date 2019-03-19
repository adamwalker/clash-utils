module BCDSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockReset)
import Test.Hspec
import Test.QuickCheck

import Data.Digits
import Clash.BCD

--BCD conversion testing
spec :: SpecWith ()
spec = do
    describe "Binary to decimal conversion using the double dabble algorithm" $ 
        it "correctly converts binary numbers between 0 and 9999 to decimal" $ property $ 
            forAll (choose (0, 9999)) $ \(x :: Int) -> --Make sure that the input range is representable within the output type
                dropWhile (==0) (Clash.toList (toDec (fromIntegral x :: BitVector 16) :: Vec 4 BCDDigit)) == map fromIntegral (digits 10 x)
    describe "ASCII <-> BCD conversion" $
        it "correctly converts between binary and ascii" $ property $
            forAll (choose (0, 9)) $ \(x :: Int) -> 
                asciiToBCD (bcdToAscii (fromIntegral x)) == Just (fromIntegral x)
    describe "BCD subtraction" $
        it "subtracts correctly" $ property prop_subtraction
    describe "BCD addition" $
        it "adds correctly" $ property prop_addition

pad :: a -> [a] -> [a]
pad def x = replicate (4 - length x) def ++ x

prop_subtraction :: Property
prop_subtraction =  forAll (choose (0, 9999)) $ \(x :: Int) -> 
        forAll (choose (0, 9999)) $ \(y :: Int) -> 
        x >= y ==> 

    let --Turn our integers into digits, padding with 0s on the left if necessary
        dsX    = pad 0 $ digits 10 x
        dsY    = pad 0 $ digits 10 y
        --Turn the lists into vectors
        inpX'  = (dsX !! 0) :> (dsX !! 1) :> (dsX !! 2) :> (dsX !! 3) :> Nil
        inpY'  = (dsY !! 0) :> (dsY !! 1) :> (dsY !! 2) :> (dsY !! 3) :> Nil
        --Expected result
        expect = digits 10 $ x - y

        inpX, inpY :: Vec 4 (BitVector 4)
        inpX = Clash.map fromIntegral inpX'
        inpY = Clash.map fromIntegral inpY'

        --Run the clash
        res = Clash.dropWhile (== 0) (map fromIntegral (Clash.toList (snd (bcdSub inpX inpY))))

    in counterexample (show expect ++ " " ++ show res) $ expect == res

prop_addition :: Property
prop_addition =  forAll (choose (0, 9999)) $ \(x :: Int) -> 
        forAll (choose (0, 9999)) $ \(y :: Int) -> 
        x + y <= 9999 ==> 

    let --Turn our integers into digits, padding with 0s on the left if necessary
        dsX    = pad 0 $ digits 10 x
        dsY    = pad 0 $ digits 10 y
        --Turn the lists into vectors
        inpX'  = (dsX !! 0) :> (dsX !! 1) :> (dsX !! 2) :> (dsX !! 3) :> Nil
        inpY'  = (dsY !! 0) :> (dsY !! 1) :> (dsY !! 2) :> (dsY !! 3) :> Nil
        --Expected result
        expect = digits 10 $ x + y

        inpX, inpY :: Vec 4 (BitVector 4)
        inpX = Clash.map fromIntegral inpX'
        inpY = Clash.map fromIntegral inpY'

        --Run the clash
        res = Clash.dropWhile (== 0) (map fromIntegral (Clash.toList (snd (bcdAdd inpX inpY))))

    in counterexample (show expect ++ " " ++ show res) $ expect == res

