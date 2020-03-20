module FieldExtractSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable, System)
import Test.Hspec
import Test.QuickCheck

import Data.Default
import Clash.Stream.FieldExtract

streamBytes :: [BitVector 8] -> [BitVector 32]
streamBytes (x : y : z : w : rest) = x ++# y ++# z ++# w : streamBytes rest
streamBytes []                     = []
streamBytes r                      = error $ "streamBytes: " ++ show r

valAtIdx :: Default a => Int -> Int -> a -> [a]
valAtIdx len idx val = map func [0..len-1]
    where
    func x
        | x == idx  = val
        | otherwise = def

fieldAtIdx :: Default a => Int -> Int -> [a] -> [a]
fieldAtIdx len idx val = replicate idx def ++ val ++ replicate (len - idx - (length val)) def

testParser 
    :: forall dom a. HiddenClockResetEnable dom 
    => (Signal dom (Unsigned 8) -> Signal dom Bool -> Signal dom (BitVector 32) -> Signal dom a)
    -> Signal dom (BitVector 32) 
    -> Signal dom a
testParser func inp = out
    where

    counter :: Signal dom (Unsigned 8)
    counter = Clash.register 0 (counter + 4)

    out     = func counter (pure True) inp

spec :: SpecWith ()
spec = describe "Message receiver" $ do

    it "byteExtract" $ 
        property $ forAll (choose (0, 255)) $ \offset -> 
            forAll (choose (offset `quot` 4 + 1, 64)) $ \pktLen -> 
                \val -> 
                    let res = simulate_lazy @System (testParser (byteExtractAccum (fromIntegral offset))) 
                            $ streamBytes 
                            $ valAtIdx ((pktLen + 1) * 4) offset val
                    in  res !! pktLen == val

    it "fieldExtract" $
        property $ forAll (choose (0, 252)) $ \offset -> 
            forAll (choose ((offset + 3) `quot` 4 + 1, 64)) $ \pktLen -> 
                \(v@(x :> y :> z :> w :> Nil) :: Vec 4 (BitVector 8)) -> 
                    let res = simulate_lazy @System (testParser (fieldExtractAccum (fromIntegral offset))) 
                            $ streamBytes 
                            $ fieldAtIdx ((pktLen + 1) * 4) offset [x, y, z, w]
                    in res !! pktLen == v

    it "byteExtractComb" $ 
        property $ forAll (choose (0, 255)) $ \offset -> 
            forAll (choose (offset `quot` 4 + 1, 64)) $ \pktLen -> 
                \val -> 
                    let res = simulate_lazy @System (testParser (byteExtractAccumComb (fromIntegral offset))) 
                            $ streamBytes 
                            $ valAtIdx (pktLen * 4) offset val 
                    in res !! (pktLen - 1) == val

    it "fieldExtractComb" $
        property $ forAll (choose (0, 252)) $ \offset -> 
            forAll (choose ((offset + 3) `quot` 4 + 1, 64)) $ \pktLen -> 
                \(v@(x :> y :> z :> w :> Nil) :: Vec 4 (BitVector 8)) -> 
                    let res = simulate_lazy @System (testParser (fieldExtractAccumComb (fromIntegral offset))) 
                            $ streamBytes 
                            $ fieldAtIdx (pktLen * 4) offset [x, y, z, w]
                    in res !! (pktLen - 1) == v

