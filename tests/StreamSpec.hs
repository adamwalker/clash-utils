module StreamSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockReset)
import Test.Hspec
import Test.QuickCheck

import Data.Default
import Data.List
import Data.Maybe
import Control.Monad
import Clash.Stream

streamize :: [a] -> [StreamIn a]
streamize []     = error "empty stream"
streamize (x:[]) = error "single element stream"
streamize (x:xs) = StreamIn True False True x : streamize' xs
    where
    streamize' []     = error "empty stream"
    streamize' (x:[]) = [StreamIn False True True x]
    streamize' (x:xs) = StreamIn False False True x : streamize' xs

stream :: [Int] -> [StreamIn Int]
stream pkt = intersperse nopStream $ intersperse nopStream $ streamize pkt ++ repeat nopStream

streamNoNop :: [Int] -> [StreamIn Int]
streamNoNop pkt = intersperse nopStream $ intersperse nopStream $ streamize pkt 

nopStream :: StreamIn Int
nopStream = StreamIn False False False 0

pktGen :: Gen [[Int]]
pktGen = do
    numMessages <- choose (1, 10)
    replicateM numMessages $ vectorOf 4 arbitrary

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
    :: forall dom gated sync a. HiddenClockReset dom gated sync 
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

    it "Single message packet" $ 
        property $ \pkt ->  
            (length pkt >= 4) ==>
                fmap Clash.toList (listToMaybe (catMaybes (take 20 $ simulate_lazy deserialize (stream pkt) :: [Maybe (Vec 4 Int)]))) == Just (take 4 pkt)

    it "Multiple message packet" $ 
        property $ forAll (choose (1, 10)) $ \numMessages -> 
            forAll (replicateM numMessages $ vectorOf 4 arbitrary) $ \messages ->
                fmap Clash.toList (catMaybes (take (4 * 4 * numMessages + 10) $ simulate_lazy deserialize (stream (concat messages)) :: [Maybe (Vec 4 Int)])) == messages

    it "Multiple packets with multiple messages" $ 
        property $ forAll (choose (1, 10)) $ \numPackets -> 
            forAll (vectorOf numPackets pktGen) $ \(messages :: [[[Int]]]) ->
                let pkts = map concat messages
                in  fmap Clash.toList (catMaybes (take (4 * length (concat pkts) + 10) $ simulate_lazy deserialize ((concat $ map streamNoNop pkts) ++ repeat nopStream) :: [Maybe (Vec 4 Int)])) == (concat messages)

    it "Selected single message" $ 
        property $ \pkt ->  
            (length pkt >= 4) ==>
                \tag -> 
                    fmap Clash.toList (listToMaybe (catMaybes (take 20 $ simulate_lazy (deserialize . selectStream (== tag)) (stream $ tag : pkt) :: [Maybe (Vec 4 Int)]))) == Just (take 4 pkt)

    it "Not selected single message" $ 
        property $ \pkt ->  
            (length pkt >= 4) ==>
                \tag -> 
                    forAll (suchThat arbitrary (/= tag)) $ \tag' -> 
                        fmap Clash.toList (listToMaybe (catMaybes (take 20 $ simulate_lazy (deserialize . selectStream (== tag)) (stream $ tag' : pkt) :: [Maybe (Vec 4 Int)]))) == Nothing

    it "Selected multiple messages" $ 
        property $ forAll (choose (1, 10)) $ \numMessages -> 
            forAll (replicateM numMessages $ vectorOf 4 arbitrary) $ \messages ->
                \tag -> 
                    fmap Clash.toList (catMaybes (take (4 * 4 * numMessages + 10) $ simulate_lazy (deserialize . selectStream (==tag)) (stream (tag : concat messages)) :: [Maybe (Vec 4 Int)])) == messages

    it "Not selected multiple messages" $ 
        property $ forAll (choose (1, 10)) $ \numMessages -> 
            forAll (replicateM numMessages $ vectorOf 4 arbitrary) $ \messages ->
                \tag -> 
                    forAll (suchThat arbitrary (/= tag)) $ \tag' -> 
                        fmap Clash.toList (catMaybes (take (4 * 4 * numMessages + 10) $ simulate_lazy (deserialize . selectStream (==tag)) (stream (tag' : concat messages)) :: [Maybe (Vec 4 Int)])) == []

    it "byteExtract" $ 
        property $ forAll (choose (0, 255)) $ \offset -> 
            forAll (choose (offset `quot` 4 + 1, 64)) $ \pktLen -> 
                \val -> 
                    ((simulate_lazy (testParser (byteExtractAccum (fromIntegral offset))) $ streamBytes $ valAtIdx ((pktLen + 1) * 4) offset val) !! pktLen) == val

    it "fieldExtract" $
        property $ forAll (choose (0, 252)) $ \offset -> 
                forAll (choose ((offset + 3) `quot` 4 + 1, 64)) $ \pktLen -> 
                    \(v@(x :> y :> z :> w :> Nil) :: Vec 4 (BitVector 8)) -> 
                        ((simulate_lazy (testParser (fieldExtractAccum (fromIntegral offset))) $ streamBytes $ fieldAtIdx ((pktLen + 1) * 4) offset [x, y, z, w]) !! pktLen) == v

    it "byteExtractComb" $ 
        property $ forAll (choose (0, 255)) $ \offset -> 
            forAll (choose (offset `quot` 4 + 1, 64)) $ \pktLen -> 
                \val -> 
                    ((simulate_lazy (testParser (byteExtractAccumComb (fromIntegral offset))) $ streamBytes $ valAtIdx (pktLen * 4) offset val) !! (pktLen - 1)) == val

    it "fieldExtractComb" $
        property $ forAll (choose (0, 252)) $ \offset -> 
            forAll (choose ((offset + 3) `quot` 4 + 1, 64)) $ \pktLen -> 
                \(v@(x :> y :> z :> w :> Nil) :: Vec 4 (BitVector 8)) -> 
                    ((simulate_lazy (testParser (fieldExtractAccumComb (fromIntegral offset))) $ streamBytes $ fieldAtIdx (pktLen * 4) offset [x, y, z, w]) !! (pktLen - 1)) == v

