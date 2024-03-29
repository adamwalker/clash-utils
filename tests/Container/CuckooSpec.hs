{-# LANGUAGE RankNTypes, DeriveGeneric, DeriveAnyClass #-}
module Container.CuckooSpec where

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
import Clash.Container.Cuckoo

spec = describe "Cuckoo hash table" $ do

    specify "Retrieves inserted elements" $ property $ forAll (choose (0, 2)) $ \idx k v -> 
        simulate_lazy @System system (testVec idx k v) !! 2 == Just (fromIntegral idx, hashFunc idx k, v)

    specify "Retrieves inserted elements with collision" $ property $ forAll (choose (0, 2)) $ \idx -> 
        forAll (elements $ [0..2] \\ [idx]) $ \collidingIdx k v k' -> 
            (k /= k') ==> 
                simulate_lazy @System system (testVecCollision idx collidingIdx k v k') !! 3 == Just (fromIntegral idx, hashFunc idx k, v)

    specify "Deletes elements" $ property $ forAll (choose (0, 2)) $ \idx k v -> 
        simulate_lazy @System system (testVecDelete idx k v) !! 3 == Nothing

    specify "Cuckoo works with randomised operations" $
        noShrinking $
        forAll (vectorOf 2500 arbitrary) $ \preInserts ->
        forAll (take 4000 <$> genOps False preInserts) $ \ops -> 
        last (sampleN @System 50000 (testHarness hashFuncs cuckoo (map (uncurry Insert) preInserts ++ ops))) == Just True

    specify "Cuckoo works with randomised operations 2" $
        noShrinking $
        forAll (vectorOf 2500 arbitrary) $ \preInserts ->
        forAll (take 4000 <$> genOps False preInserts) $ \ops -> 
        last (sampleN @System 50000 (testHarness hashFuncs cuckoo2 (map (uncurry Insert) preInserts ++ ops))) == Just True

    specify "Cuckoo works with randomised operations and recently modified keys" $
        noShrinking $
        forAll (vectorOf 2500 arbitrary) $ \preInserts ->
        forAll (take 4000 <$> genOps True preInserts) $ \ops -> 
        last (sampleN @System 50000 (testHarness hashFuncs cuckoo (map (uncurry Insert) preInserts ++ ops))) == Just True

    specify "Cuckoo works with randomised operations and recently modified keys 2" $
        noShrinking $
        forAll (vectorOf 2500 arbitrary) $ \preInserts ->
        forAll (take 4000 <$> genOps True preInserts) $ \ops -> 
        last (sampleN @System 50000 (testHarness hashFuncs cuckoo2 (map (uncurry Insert) preInserts ++ ops))) == Just True

    specify "Cuckoo works with high load"               $ property $ noShrinking $ testInserts cuckoo

    specify "Cuckoo works with high load 2"             $ property $ noShrinking $ testInserts cuckoo2

system 
    :: HiddenClockResetEnable dom
    => Signal dom (
            Vec 3 (Maybe (Unsigned 12, Maybe (TableEntry String String))), 
            String
        ) 
    -> Signal dom (Maybe (Index 3, Unsigned 12, String))
system inp = cuckooLookup (\val -> Clash.map (flip hashFunc val) (0 :> 1 :> 2 :> Nil)) (sequenceA writes) req
    where
    (writes, req) = unbundle inp

hashFunc :: Int -> String -> Unsigned 12
hashFunc salt = fromIntegral . hashWithSalt salt

testVec 
    :: Int 
    -> String 
    -> String 
    -> [(
            Vec 3 (Maybe (Unsigned 12, Maybe (TableEntry String String))), 
            String
        )]
testVec idx k v = [write, lookup, null]
    where
    write  = (Clash.replace idx (Just (hashFunc idx k, Just (TableEntry k v))) (Clash.repeat Nothing), "")
    lookup = (Clash.repeat Nothing, k)
    null   = (Clash.repeat Nothing, "")

testVecCollision 
    :: Int 
    -> Int 
    -> String 
    -> String 
    -> String 
    -> [(
            Vec 3 (Maybe (Unsigned 12, Maybe (TableEntry String String))), 
            String
        )]
testVecCollision idx collidingIdx k v k' = [write, writeCollision, lookup, null]
    where
    write          = (Clash.replace idx          (Just (hashFunc idx          k, Just (TableEntry k v)))       (Clash.repeat Nothing), "")
    writeCollision = (Clash.replace collidingIdx (Just (hashFunc collidingIdx k, Just (TableEntry k' "asdf"))) (Clash.repeat Nothing), "")
    lookup         = (Clash.repeat Nothing, k)
    null           = (Clash.repeat Nothing, "")

testVecDelete
    :: Int 
    -> String 
    -> String 
    -> [(
            Vec 3 (Maybe (Unsigned 12, Maybe (TableEntry String String))), 
            String
        )]
testVecDelete idx k v = [write, delete, lookup, null]
    where
    write  = (Clash.replace idx (Just (hashFunc idx k, Just (TableEntry k v))) (Clash.repeat Nothing), "")
    delete = (Clash.replace idx (Just (hashFunc idx k, Nothing))               (Clash.repeat Nothing), "")
    lookup = (Clash.repeat Nothing, k)
    null   = (Clash.repeat Nothing, "")

type Cuckoo = forall dom cnt m n k v. (HiddenClockResetEnable dom, KnownNat m, KnownNat n, Eq k, KnownNat cnt, NFDataX k, NFDataX v)
    => (k -> Vec (m + 1) (Unsigned n))                                       
    -> Signal dom k                                                          
    -> Signal dom v
    -> Signal dom Bool
    -> Signal dom Bool
    -> (
        Signal dom (Maybe (Index (m + 1), Unsigned n, v)), -- Table index, table row, value
        Signal dom Bool, 
        Signal dom Bool,
        Signal dom (Unsigned cnt),
        Signal dom (Unsigned cnt)
        )

cuckoo2
    :: forall dom cnt m n k v. (HiddenClockResetEnable dom, KnownNat m, KnownNat n, Eq k, KnownNat cnt, NFDataX k, NFDataX v)
    => (k -> Vec (m + 1) (Unsigned n))                                       
    -> Signal dom k                                                          
    -> Signal dom v
    -> Signal dom Bool
    -> Signal dom Bool
    -> (
        Signal dom (Maybe (Index (m + 1), Unsigned n, v)), -- Table index, table row, value
        Signal dom Bool, 
        Signal dom Bool,
        Signal dom (Unsigned cnt),
        Signal dom (Unsigned cnt)
        )
cuckoo2 hashFunctions key' value' insert delete = (lookupResult, inserting, insertingDone, insertIters, nItems)
    where
    toHash :: Signal dom k
    toHash =  mux
        hashRequest
        (key <$> evictedEntry)
        key'

    --Calculate the lookup hashes
    hashes :: Vec (m + 1) (Signal dom (Unsigned n))
    hashes =  sequenceA $ hashFunctions <$> toHash

    (lookupResult, inserting, insertingDone, evictedEntry, hashRequest, insertIters, nItems) = cuckoo' hashes key' value' insert delete hashRequest

insertTestHarness
    :: forall dom. HiddenClockResetEnable dom
    => Cuckoo
    -> [(BitVector 32, BitVector 32)]
    -> Signal dom ((Bool, Bool), Unsigned 16)
insertTestHarness cuckoo vals = bundle (bundle (register True (register True insertingPhase) .||. success, done), numIters)
    where

    inserted :: Signal dom Int
    inserted = regEn 0 (inserting .&&. insertingDone) (inserted + 1)

    maxInserts :: Signal dom (Unsigned 16)
    maxInserts =  register 0 $ func <$> insertingDone <*> numIters <*> maxInserts
        where
        func False _ max' = max'
        func True  n max' = max n max'
    
    (lookupResult, inserting, insertingDone, numIters, nItems) = cuckoo hashFuncs insertKey insertValue insert (pure False)

    hashFuncs :: BitVector 32 -> Vec 3 (Unsigned 10)
    --hashFuncs x = h 0 :> h 1 :> h 2 :> Nil
    --    where
    --    h i = fromIntegral $ hashWithSalt i $ Prelude.map (fromIntegral :: BitVector 8 -> Int) [
    --            slice (SNat @ 31) (SNat @ 24) x, 
    --            slice (SNat @ 23) (SNat @ 16) x, 
    --            slice (SNat @ 15) (SNat @ 8)  x, 
    --            slice (SNat @ 7)  (SNat @ 0)  x
    --        ]

    hashFuncs x = unpack (slice (SNat @9) (SNat @0) crc) :> unpack (slice (SNat @19) (SNat @10) crc) :> unpack (slice (SNat @29) (SNat @20) crc) :> Nil
        where
        crc :: Unsigned 32
        crc = fromIntegral $ crc32 $ BS.pack $ Prelude.map fromIntegral [
                slice (SNat @31) (SNat @24) x, 
                slice (SNat @23) (SNat @16) x, 
                slice (SNat @15) (SNat @8)  x, 
                slice (SNat @7)  (SNat @0)  x
            ]

    (insertKey', insertValue, insert') = unbundle $ mealy step vals inserting
        where
        step :: [(BitVector 32, BitVector 32)] -> Bool -> ([(BitVector 32, BitVector 32)], (BitVector 32, BitVector 32, Bool))
        step []             _     = ([],   (0, 0, False))
        step ((k,v) : rest) False = (rest, (k, v, True))
        step st             True  = (st,   (0, 0, False))

    numItems = 2700

    insertingPhase = (inserted .<. pure numItems) .||. inserting
    insert         = insert' .&&. insertingPhase
    insertKey      = mux insertingPhase insertKey' toLookup

    (done, lookupsRemaining, toLookup) = unbundle $ register (False, Prelude.take numItems vals, 0) $ func <$> insertingPhase <*> lookupsRemaining
        where
        func True  lookupsRemaining = (False, lookupsRemaining, 0)
        func False []               = (True,  [], 0)
        func False (x:xs)           = (False, xs, fst x)
    expect                   = register Nothing $ register Nothing $ flip Map.lookup (Map.fromList (Prelude.take numItems vals)) <$> insertKey
    success                  = expect .==. fmap (fmap sel3) lookupResult

testInserts :: Cuckoo -> InfiniteList (BitVector 32, BitVector 32) -> Property
testInserts cuckoo (InfiniteList insertSeq _) = collect (last maxIterations) $ finished && success
    where
    (res, maxIterations) = unzip $ sampleN_lazy @System 50000 $ insertTestHarness cuckoo insertSeq
    finished             = elem True $ Prelude.map snd res
    success              = all id $ Prelude.map fst res

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

        testCases = concat [
                forEmpty,
                if Map.null accum                         then [] else forNonempty,
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
                    GenState (Map.insert key value accum) (key : take 2 lastModifications)
                )
        --Insert a key that is already contained in the map
        insertExists = do
            key   <- elements $ Map.keys accum
            value <- arbitrary
            return (
                    Insert key value, 
                    GenState (Map.insert key value accum) (key : take 2 lastModifications)
                )
        --Insert a key that was recently inserted
        insertRecentMod = do
            key   <- elements lastModifications
            value <- arbitrary
            return (
                    Insert key value, 
                    GenState (Map.insert key value accum) (key : take 2 lastModifications)
                )
        --Delete a random key 
        deleteAny = do
            key   <- arbitrary
            return (
                    Delete key, 
                    GenState (Map.delete key accum) (key : take 2 lastModifications)
                )
        --Delete a key that is contained in the map
        deleteExists = do
            key   <- elements $ Map.keys accum
            return (
                    Delete key, 
                    GenState (Map.delete key accum) (key : take 2 lastModifications)
                )
        --Delete a key that was recently inserted
        deleteRecentMod = do
            key   <- elements lastModifications
            return (
                    Delete key, 
                    GenState (Map.delete key accum) (key : take 2 lastModifications)
                )

data OpState
    = LookupState (Maybe String) Bool
    | InsertState 
    | IdleState
    deriving (Generic, NFDataX)

data TestbenchState key
    = Success
    | Failure
    | InProgress OpState [Op key] --State of current operation, remaining operations
    deriving (Generic, NFDataX)

hashFuncs :: [Word8] -> Vec 3 (Unsigned 10)
hashFuncs x = Clash.map (\idx -> fromIntegral $ (`mod` 1024) $ hashWithSalt idx x) (iterateI (+1) 0)

testHarness 
    :: forall dom key. (HiddenClockResetEnable dom, Ord key, Default key, NFDataX key, NFDataX (TestbenchState key))
    => (key -> Vec 3 (Unsigned 10))
    -> Cuckoo
    -> [Op key] 
    -> Signal dom (Maybe Bool)
testHarness hashFuncs cuckoo ops = result
    where

    --DUT
    (lookupVal, insertDone, _, _, _ :: Signal dom (Unsigned 16)) = cuckoo hashFuncs key value insert delete

    emptyControlSigs :: (key, String, Bool, Bool)
    emptyControlSigs = (def, "", False, False)

    (result, controlSignals) = mealyB step initState (fmap sel3 <$> lookupVal, insertDone)
    (key, value, insert, delete) = unbundle controlSignals

    initState = InProgress IdleState ops

    step :: TestbenchState key -> (Maybe String, Bool) -> (TestbenchState key, (Maybe Bool, (key, String, Bool, Bool)))
    step Success _ = (Success, (Just True,  emptyControlSigs))
    step Failure _ = (Failure, (Just False, emptyControlSigs))

    step (InProgress IdleState ops) _ = (ns, (Nothing, out))
        where (out, ns) = nextState ops

    step st@(InProgress InsertState ops) (_, insertDone)
        | insertDone = (st,                       (Nothing, emptyControlSigs))
        | otherwise  = (InProgress IdleState ops, (Nothing, emptyControlSigs))

    step (InProgress (LookupState val lookupReady) ops) (lookupVal, _) 
        | not lookupReady  = (InProgress (LookupState val True) ops, (Nothing, emptyControlSigs))
        | val == lookupVal = (ns,      (Nothing, out))
        | otherwise        = (Failure, (Nothing, emptyControlSigs)) 
        where (out, ns) = nextState ops

    nextState []                     = (emptyControlSigs,         Success)
    nextState (Lookup key val : ops) = ((key, "",  False, False), InProgress (LookupState val False) ops)
    nextState (Insert key val : ops) = ((key, val, True,  False), InProgress InsertState             ops)
    nextState (Delete key     : ops) = ((key, "",  False, True),  InProgress IdleState               (Idle : Idle : ops))
    nextState (Idle           : ops) = (emptyControlSigs,         InProgress IdleState               ops)

