{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module PriorityQueueSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable, toList, sample, fromList, System, type (+), NFDataX, type (<=), d4, d7, d1, d2, d8, errorX)
import GHC.Generics
import Test.Hspec
import Test.QuickCheck hiding (sample)
import Data.Default

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List
import Control.Monad.State

import Clash.Container.PriorityQueue

spec :: SpecWith ()
spec = do
    describe "Priority Queue" $ do
        it "random operations"                         $ property prop_randomOps
        it "random operations cascade"                 $ property prop_randomOpsCascade
        it "delete all but last chunk"                 $ property prop_deleteAllButLast
        it "delete all but last chunk + cascade"       $ property prop_deleteAllButLastCascade
        it "delete all but half of last chunk"         $ property prop_deleteLastLeavingFour
        it "delete all but half of last chunk cascade" $ property prop_deleteLastLeavingFourCascade

data Op a
    = Insert a
    | Delete a
    deriving (Generic, NFDataX, Show)

genOp :: (Ord a, Arbitrary a) => Set a -> Gen (Op a, Set a)
genOp current = case Set.null current of
    True  -> insertAny
    False -> oneof [insertAny, insertAny, deleteExists, deleteEarly]
    where
    insertAny    = do
        val <- arbitrary `suchThat` (\x -> not $ x `elem` current)
        return (Insert val, Set.insert val current)
    deleteExists = do
        val <- elements (Set.toList current)
        return (Delete val, Set.delete val current)
    deleteEarly = do
        val <- elements $ take 8 (Set.toList current)
        return (Delete val, Set.delete val current)

uniques :: Eq a => Gen a -> Gen [a]
uniques gen = fmap Data.List.nub $ listOf gen

genOps :: forall a. (Ord a, Arbitrary a) => Int -> Int -> Gen ([Op a], Set a)
genOps numInitial numOps = do
    initial        <- take numInitial <$> uniques arbitrary
    (rest, setOut) <- runStateT (sequence (replicate numOps (StateT genOp)) :: StateT (Set a) Gen [Op a]) (Set.fromList initial)
    return $ (map Insert initial ++ rest, setOut)

streamList 
    :: (HiddenClockResetEnable dom, NFDataX a)
    => [Op a]
    -> Signal dom Bool
    -> (Signal dom Bool, Signal dom Bool, Signal dom a, Signal dom Bool)
streamList ops busy = unbundle $ mealy step ops busy
    where
    step :: [Op a] -> Bool -> ([Op a], (Bool, Bool, a, Bool))
    step l@(x:xs)          True  = (l,  (False, False, errorX "invalid insert", False))
    step   ((Insert x):xs) False = (xs, (True,  False, x,                       False))
    step   ((Delete x):xs) False = (xs, (False, True,  x,                       False))
    step   []              _     = ([], (False, False, errorX "invalid insert", True))

system 
    :: (HiddenClockResetEnable dom, NFDataX a, Bounded a, Ord a, Default a, 1 <= n, KnownNat m, Num a)
    => SNat n
    -> SNat m
    -> SNat (c + 1)
    -> [Op a] 
    -> (Signal dom (Vec (m + 1) a), Signal dom Bool, Signal dom Bool)
system iters SNat cascade xs = (res, finished, busy)
    where
    (insert, delete, val, finished) = streamList xs busy
    (res, busy)                     = fst $ prioQueueMultiRam cascade iters (pure (1 :: BitVector 8)) insert delete val val (pure maxBound)

pad :: Int -> a -> [a] -> [a]
pad num def x = x ++ replicate (num - length x) def 

prop_template iters width cascade ops expect =  
    let res = fmap (\(x, y, z) -> toList x)
            $ Data.List.find (\(x, y, z) -> y && not z) 
            $ sample @System 
            $ bundle 
            $ system iters width cascade ops
    in Just expect === res

prop_randomOps 
    = forAll (genOps @Int 32 32) $ \(ops, set) -> 
        prop_template d4 d7 d1 ops (pad 8 maxBound $ take 8 $ Set.toList set)

prop_randomOpsCascade
    = forAll (genOps @Int 64 64) $ \(ops, set) -> 
        prop_template d4 d7 d2 ops (pad 8 maxBound $ take 8 $ Set.toList set)

prop_deleteAllButLast 
    = forAll (shuffle [0..63]) $ \inserts -> 
        forAll (shuffle [0..55]) $ \deletes -> 
            prop_template d8 d7 d1 
                (map Insert inserts ++ map Delete deletes) 
                [56, 57, 58, 59, 60, 61, 62, 63 :: Int]

prop_deleteLastLeavingFour
    = forAll (shuffle [0..63]) $ \inserts -> 
        forAll (shuffle ([0..51] ++ [60, 61, 62, 63]) ) $ \deletes -> 
            prop_template d8 d7 d1 
                (map Insert inserts ++ map Delete deletes) 
                [52, 53, 54, 55, 56, 57, 58, 59 :: Int]

prop_deleteAllButLastCascade
    = forAll (shuffle [0..63]) $ \inserts -> 
        forAll (shuffle [0..55]) $ \deletes -> 
            prop_template d4 d7 d2 
                (map Insert inserts ++ map Delete deletes) 
                [56, 57, 58, 59, 60, 61, 62, 63 :: Int]

prop_deleteLastLeavingFourCascade
    = forAll (shuffle [0..63]) $ \inserts -> 
        forAll (shuffle ([0..51] ++ [60, 61, 62, 63]) ) $ \deletes -> 
            prop_template d4 d7 d2 
                (map Insert inserts ++ map Delete deletes) 
                [52, 53, 54, 55, 56, 57, 58, 59 :: Int]
        
