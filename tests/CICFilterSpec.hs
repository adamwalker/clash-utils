module CICFilterSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable, type (+), extend, fromList, sample, System)
import Test.Hspec
import Test.QuickCheck hiding (sample)

import Clash.DSP.CICFilter

spec = describe "CIC filters" $ do
    specify "1st order is equivalent to moving sum" $ property $ prop_cicSums
    specify "2nd order" $ property $ prop_cicSecondOrder

groups :: Int -> Int -> [a] -> [[a]]
groups t d x = take t x : groups t d (drop d x)

prop_cicSums :: InfiniteList (BitVector 8) -> Bool
prop_cicSums (InfiniteList x _) = expect == result
    where
    extended :: [BitVector 11]
    extended =  map extend x
    expect   
        = take 64 
        $ map sum 
        $ groups 8 4 extended
    result   
        = take 64 
        $ drop 4 
        $ map snd 
        $ filter fst 
        $ sample @System
        $ bundle 
        $ cicDecimate (SNat @ 4) (SNat @ 2) (SNat @ 1) (pure True) (fromList (0 : extended))

prop_cicSecondOrder :: InfiniteList (BitVector 8) -> Bool
prop_cicSecondOrder (InfiniteList x _) = expect == result
    where
    extended :: [BitVector 14]
    extended =  map extend x
    expect   
        = take 64 
        $ map sum 
        $ groups 8 4
        $ map sum
        $ groups 8 1 extended
    result   
        = take 64 
        $ drop 7
        $ map snd 
        $ filter fst 
        $ sample @System
        $ bundle 
        $ cicDecimate (SNat @ 4) (SNat @ 2) (SNat @ 2) (pure True) (fromList (0 : extended))
