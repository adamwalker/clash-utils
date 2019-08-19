module MiscSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable, replace, findIndex)
import Test.Hspec
import Test.QuickCheck

import Clash.Misc

--Misc
spec = describe "Misc utilities" $ do
    specify "slice equals software implementation"               $ property prop_slice
    specify "reversing bitvector twice gives original bitvector" $ property prop_revBV
    specify "swapping endianness twice gives original bitvector" $ property prop_swapEndian
    specify "Priority select using the carry chain works"        $ property prop_prioSelectCarryChain

refSlice :: Int -> [a] -> [a] -> [a]
refSlice idx dat vec
    | idx + length dat > length vec
        = take idx vec ++ take (length vec - idx) dat
    | otherwise
        = take idx vec ++ dat ++ drop (idx + length dat) vec

prop_slice :: Index 253 -> Vec 21 Int -> Vec 253 Int -> Bool
prop_slice startIdx dat vec = Clash.toList (replaceSlice startIdx dat vec) == refSlice (fromIntegral startIdx) (Clash.toList dat) (Clash.toList vec)

prop_revBV :: BitVector 253 -> Bool
prop_revBV x = x == revBV (revBV x)

prop_swapEndian :: BitVector 256 -> Bool
prop_swapEndian x = x == swapEndian (swapEndian x)

prop_prioSelectCarryChain :: Vec 32 Bool -> Bool
prop_prioSelectCarryChain x = result == expect
    where
    result = prioSelectCarryChain x
    expect = case findIndex id x of
        Nothing  -> Clash.repeat False
        Just idx -> replace idx True $ Clash.repeat False
