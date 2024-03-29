{-# LANGUAGE RankNTypes, DeriveGeneric, DeriveAnyClass #-}
module Container.CuckooPipelineSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable, (.==.), sampleN_lazy, register, regEn, (.<.), (.||.), (.&&.), iterateI, mealyB, sampleN, slice, type (+), errorX, NFDataX,
                      System)

import Test.Hspec
import Test.QuickCheck hiding ((.||.), (.&&.), Success, Failure)

import Data.Hashable
import Data.List
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Tuple.All
import qualified Data.ByteString as BS
import Data.Digest.CRC32
import Data.Default
import Data.Word
import GHC.Generics

import Clash.ErrorControl.CRC
import Clash.Container.CuckooPipeline

spec = describe "CuckooPipeline hash table" $ do

    specify "Works with randomised operations" $
        noShrinking $
        forAll (vectorOf 2500 arbitrary) $ \preInserts ->
        forAll (take 4000 <$> genOps False preInserts) $ \ops -> 
        last (sampleN @System 50000 (testHarness 0 hashFuncs (cuckooPipelineInsert (SNat @0)) (map (uncurry Insert) preInserts ++ ops))) == Just True

    specify "Works with randomised operations and recently modified keys" $
        noShrinking $
        forAll (vectorOf 2500 arbitrary) $ \preInserts ->
        forAll (take 4000 <$> genOps True preInserts) $ \ops -> 
        last (sampleN @System 50000 (testHarness 0 hashFuncs (cuckooPipelineInsert (SNat @0)) (map (uncurry Insert) preInserts ++ ops))) == Just True

    specify "Works with randomised operations and a pipeline stage" $
        noShrinking $
        forAll (vectorOf 2500 arbitrary) $ \preInserts ->
        forAll (take 4000 <$> genOps False preInserts) $ \ops -> 
        last (sampleN @System 50000 (testHarness 1 hashFuncs (cuckooPipelineInsert (SNat @1)) (map (uncurry Insert) preInserts ++ ops))) == Just True

    specify "Works with randomised operations and recently modified keys and a pipeline stage" $
        noShrinking $
        forAll (vectorOf 2500 arbitrary) $ \preInserts ->
        forAll (take 4000 <$> genOps True preInserts) $ \ops -> 
        last (sampleN @System 50000 (testHarness 1 hashFuncs (cuckooPipelineInsert (SNat @1)) (map (uncurry Insert) preInserts ++ ops))) == Just True

data Op key
    = Lookup key (Maybe String)
    | Insert key String
    | Delete key
    | Idle
    deriving (Show, Generic, NFDataX)

data GenState key = GenState {
    theMap            :: Map key String,
    lastModifications :: [key]
}

genOps :: forall key. (Ord key, Arbitrary key) => Bool -> [(key, String)] -> Gen [Op key]
genOps doRecent initial = genOps' $ GenState (Map.fromList initial) []
    where
    genOps' :: GenState key -> Gen [Op key]
    genOps' (GenState accum lastModifications) = do
        (res, accum') <- oneof testCases
        rest <- genOps' accum'
        return $ res : rest

        where

        testCases = Prelude.concat [
                forEmpty,
                if Map.null accum then [] else forNonempty,
                if not doRecent || null lastModifications then [] else forHasModifications
            ]
        
        forEmpty = [
                idle,
                lookupAny,
                insertAny,
                deleteAny
            ]

        forNonempty = [
                lookupExists,
                insertExists,
                deleteExists
            ]

        forHasModifications = [
                lookupRecentMod,
                insertRecentMod,
                deleteRecentMod
            ]

        --No operations this cycle
        idle = pure (Idle, GenState accum lastModifications) 
        --Lookup a random key 
        lookupAny = do
            key   <- arbitrary
            return (
                    Lookup key (Map.lookup key accum), 
                    GenState accum lastModifications
                )
        --Lookup a key that is in the map
        lookupExists = do
            key   <- elements $ Map.keys accum
            return (
                    Lookup key (Map.lookup key accum), 
                    GenState accum lastModifications
                )
        --Lookup a recently inserted item
        lookupRecentMod = do
            key   <- elements lastModifications 
            return (
                    Lookup key (Map.lookup key accum), 
                    GenState accum lastModifications
                )
        --Insert a random key and value
        insertAny = do
            key   <- arbitrary
            value <- arbitrary
            return (
                    Insert key value, 
                    GenState (Map.insert key value accum) (key : Prelude.take 2 lastModifications)
                )
        --Insert a key that is already contained in the map
        insertExists = do
            key   <- elements $ Map.keys accum
            value <- arbitrary
            return (
                    Insert key value, 
                    GenState (Map.insert key value accum) (key : Prelude.take 2 lastModifications)
                )
        --Insert a key that was recently inserted
        insertRecentMod = do
            key   <- elements lastModifications
            value <- arbitrary
            return (
                    Insert key value, 
                    GenState (Map.insert key value accum) (key : Prelude.take 2 lastModifications)
                )
        --Delete a random key 
        deleteAny = do
            key   <- arbitrary
            return (
                    Delete key, 
                    GenState (Map.delete key accum) (key : Prelude.take 2 lastModifications)
                )
        --Delete a key that is contained in the map
        deleteExists = do
            key   <- elements $ Map.keys accum
            return (
                    Delete key, 
                    GenState (Map.delete key accum) (key : Prelude.take 2 lastModifications)
                )
        --Delete a key that was recently inserted
        deleteRecentMod = do
            key   <- elements lastModifications
            return (
                    Delete key, 
                    GenState (Map.delete key accum) (key : Prelude.take 2 lastModifications)
                )

data OpState
    = LookupState Int (Maybe String)
    | IdleState
    deriving (Generic, NFDataX)

data TestbenchState key
    = Success
    | Failure
    | InProgress OpState [Op key] --State of current operation, remaining operations
    deriving (Generic, NFDataX)

type Cuckoo = forall dom m n k v. (HiddenClockResetEnable dom, KnownNat m, KnownNat n, Eq k, NFDataX k, NFDataX v)
    => Vec (m + 1) (k -> Unsigned n)
    -> Signal dom k                                                          
    -> Signal dom (Maybe (Maybe v))
    -> (
        Signal dom (Maybe v),
        Signal dom Bool
        )

testHarness 
    :: forall dom key. (HiddenClockResetEnable dom, Ord key, Default key, NFDataX key)
    => Int
    -> (Vec 3 (key -> Unsigned 10))
    -> Cuckoo
    -> [Op key] 
    -> Signal dom (Maybe Bool)
testHarness readLatency hashFuncs cuckoo ops = result
    where

    --DUT
    modification = mux delete (pure (Just Nothing)) (mux insert (Just . Just <$> value) (pure Nothing))
    (lookupVal, busy) = cuckoo hashFuncs key modification 

    --TB
    emptyControlSigs :: (key, String, Bool, Bool)
    emptyControlSigs = (def, "", False, False)

    (result, controlSignals) = mealyB step initState (lookupVal, busy)
    (key, value, insert, delete) = unbundle controlSignals

    initState = InProgress IdleState ops

    step :: TestbenchState key -> (Maybe String, Bool) -> (TestbenchState key, (Maybe Bool, (key, String, Bool, Bool)))
    step Success _ = (Success, (Just True,  emptyControlSigs))
    step Failure _ = (Failure, (Just False, emptyControlSigs))

    step st (_, True) = (st, (Nothing, emptyControlSigs))

    step (InProgress st ops) (lookupVal, _) = step' st
        where
        step' IdleState         = (ns, (Nothing, out))
        step' (LookupState 0 val) 
            | val == lookupVal = (ns,      (Nothing, out))
            | otherwise        = (Failure, (Nothing, emptyControlSigs)) 
        step' (LookupState wait val)
            = (InProgress (LookupState (wait - 1) val) ops, (Nothing, emptyControlSigs))
        (out, ns) = nextState ops

    nextState []                     = (emptyControlSigs,         Success)
    nextState (Lookup key val : ops) = ((key, "",  False, False), InProgress (LookupState readLatency val) ops)
    nextState (Insert key val : ops) = ((key, val, True,  False), InProgress IdleState           ops)
    nextState (Delete key     : ops) = ((key, "",  False, True),  InProgress IdleState           ops)
    nextState (Idle           : ops) = (emptyControlSigs,         InProgress IdleState           ops)

hashFuncs :: Vec 3 ([Word8] -> Unsigned 10)
hashFuncs = Clash.map (\idx x -> fromIntegral $ (`mod` 1024) $ hashWithSalt idx x) (iterateI (+1) 0)

