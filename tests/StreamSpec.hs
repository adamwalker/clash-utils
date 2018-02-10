module StreamSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HasClockReset)
import Test.Hspec
import Test.QuickCheck

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

nopStream :: StreamIn Int
nopStream = StreamIn False False False 0

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


