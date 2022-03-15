module Arithmetic.MultiplierSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable, extend, fromList, toList, sample, bitCoerce, System)
import Test.Hspec
import Test.QuickCheck hiding (sample)

import Clash.Arithmetic.Multiplier

spec = describe "Multiplier lookahead" $ do
    specify "unsigned"                 $ property prop_unsigned
    specify "signed"                   $ property prop_signed
    specify "serial carry save"        $ property prop_serialMultiplyCarrySave
    specify "serial carry save signed" $ property prop_serialMultiplyCarrySaveSigned
    specify "booth Multiplier"         $ property prop_booth

prop_unsigned :: BitVector 8 -> BitVector 8 -> Property
prop_unsigned x y = multiply x y === (extend x * extend y)

prop_signed :: BitVector 8 -> BitVector 8 -> Property
prop_signed x y = multiplySigned x y === pack ((extend (unpack x) :: Signed 16) * extend (unpack y))

prop_serialMultiplyCarrySave :: BitVector 8 -> BitVector 8 -> Property
prop_serialMultiplyCarrySave x y = result === expect
    where

    expect = reverse $ toList (unpack ((extend x :: BitVector 16) * extend y) :: Vec 16 Bool)

    y' = reverse $ toList (unpack y :: Vec 8 Bool)

    result 
        = take 16 
        $ drop 2 
        $ sample @System
        $ serialMultiplyCarrySave (pure x) (pure True)
        $ fromList $ False : y' ++ repeat False 

prop_serialMultiplyCarrySaveSigned :: BitVector 8 -> BitVector 8 -> Property
prop_serialMultiplyCarrySaveSigned x y = result === expect
    where

    expect = reverse $ toList (bitCoerce ((extend (unpack x) :: Signed 16) * extend (unpack y)) :: Vec 16 Bool)

    y' = reverse $ toList (unpack y :: Vec 8 Bool)

    msb = fromList $ [False, False, False, False, False, False, False, False, True] ++ repeat False

    result 
        = take 16 
        $ drop 2 
        $ sample @System
        $ serialMultiplyCarrySaveSigned (pure x) (pure True) msb
        $ fromList $ False : y' ++ repeat False 

prop_booth :: BitVector 7 -> BitVector 7 -> Property
prop_booth x y = boothMultiply x y === (extend x * extend y)

