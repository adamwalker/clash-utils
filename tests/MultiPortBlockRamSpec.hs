module MultiPortBlockRamSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable, extend, fromList, toList, sample, bitCoerce, System, NFDataX, UNat(..), type (+), type (<=), register, toUNat)
import Test.Hspec
import Test.QuickCheck hiding (sample)

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)

import Data.Maybe

import Clash.Container.MultiPortBlockRam

spec = describe "Multi port block ram" $ do
    specify "is equivalent to golden model" $ property prop_multiPortBlockRam

--TODO: this is silly
instance NFDataX (IntMap a)
    where 
    deepErrorX = const (IntMap.empty)
    rnfX = const ()

goldenMultiPortBlockRam 
    :: forall dom nWrites nWrites' nReads n addrBits
    .  (HiddenClockResetEnable dom, nWrites ~ (nWrites' + 1), KnownNat n, KnownNat addrBits, KnownNat nWrites, 1 <= nReads, KnownNat nReads)
    => Vec nReads (Signal dom (Unsigned addrBits))
    -> Vec nWrites (Signal dom (Maybe (Unsigned addrBits, BitVector n)))
    -> Vec nReads (Signal dom (BitVector n))
goldenMultiPortBlockRam reads writes = Clash.map (register 0) readResults
    where

    readResults :: Vec nReads (Signal dom (BitVector n))
    readResults =  sequenceA $ func <$> state <*> sequenceA reads
        where
        func :: IntMap (BitVector n) -> Vec nReads (Unsigned addrBits) -> Vec nReads (BitVector n)
        func state reads = Clash.map (fromMaybe 0 . flip IntMap.lookup state . fromIntegral) reads
    
    state :: Signal dom (IntMap (BitVector n))
    state =  register IntMap.empty $ step <$> state <*> sequenceA writes
        where
        step :: IntMap (BitVector n) -> Vec nWrites (Maybe (Unsigned addrBits, BitVector n)) -> IntMap (BitVector n)
        step state writes = Clash.foldl func state writes
            where
            func :: IntMap (BitVector n) -> Maybe (Unsigned addrBits, BitVector n) -> IntMap (BitVector n)
            func state Nothing            = state
            func state (Just (addr, dat)) = IntMap.insert (fromIntegral addr) dat state

genDistinctWrites :: forall n. KnownNat n => Gen (Vec n (Maybe (Unsigned 6, BitVector 32)))
genDistinctWrites = genDistinct' Set.empty (toUNat (SNat @ n))
    where
    genDistinct' :: forall n. Set (Unsigned 6) -> UNat n -> Gen (Vec n (Maybe (Unsigned 6, BitVector 32)))
    genDistinct' _    UZero      = pure Nil
    genDistinct' seen (USucc gs) = do
        let func = do
                res <- oneof [
                        pure Nothing,
                        Just <$> ((,) <$> arbitrary <*> arbitrary)
                    ]
                case res of
                    Nothing -> return (seen, res)
                    Just (addr, _)  -> do
                        case addr `elem` seen of
                            True  -> func
                            False -> return (Set.insert addr seen, res)

        (next, res) <- func
        rest <- genDistinct' next gs
        return $ res :> rest

prop_multiPortBlockRam 
    = forAll (infiniteListOf (genDistinctWrites @ 4)) $ \writes -> 
        forAll (infiniteListOf arbitrary) $ \(reads :: [Vec 4 (Unsigned 6)]) -> 
            let 
                expect = take 1000 $ drop 1 $ sample @System $ sequenceA $ goldenMultiPortBlockRam (sequenceA $ fromList reads) (sequenceA $ fromList writes)
                result = take 1000 $ drop 1 $ sample @System $ sequenceA $ multiPortBlockRam       (sequenceA $ fromList reads) (sequenceA $ fromList writes)
            in  result == expect

