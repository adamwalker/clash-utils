module CICFilterSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockReset, type (+), extend, Undefined, fromList, sample)
import Test.Hspec
import Test.QuickCheck hiding (sample)

import Clash.DSP.CICFilter

spec = describe "CIC filters" $ do
    specify "1st order is equivalent to moving sum" $ property $ prop_cic_sums

groups :: Int -> Int -> [a] -> [[a]]
groups t d x = take t x : groups t d (drop d x)

prop_cic_sums :: InfiniteList (BitVector 8) -> Bool
prop_cic_sums (InfiniteList x _) = expect == result
    where
    extended :: [BitVector 10]
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
        $ sample 
        $ bundle 
        $ cicDecimate (SNat @ 4) (SNat @ 2) (SNat @ 1) (pure True) (fromList (0 : extended))
