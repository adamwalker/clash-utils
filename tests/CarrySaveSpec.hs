module CarrySaveSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable, extend, shiftL)
import Test.Hspec
import Test.QuickCheck

import Clash.Arithmetic.CarrySave

--Misc
spec = describe "Carry save" $ do
    specify "is correct" $ property prop_carrySave

prop_carrySave :: Vec 4 (BitVector 8) -> Bool
prop_carrySave xs = expect == result
    where
    (carry :> Nil, (r1 :> r2 :> Nil)) = compressorN (Clash.repeat False) xs 
    expect :: BitVector 10 
    expect =  sum $ Clash.map extend xs
    result :: BitVector 10
    result =  (extend r1 `shiftL` 1) + extend r2 + (extend (pack carry) `shiftL` 8)

