module ScramblerSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HasClockReset)
import Test.Hspec
import Test.QuickCheck

import Clash.Scrambler

--Scrambler
spec = describe "Scrambler" $ 
    specify "descramber . scrambler == id" $ property prop_scrambler

    where
    prop_scrambler :: BitVector 20 -> BitVector 19 -> [Bool] -> Bool
    prop_scrambler initial (poly :: BitVector 19) input = take (length input) (simulate combined input) == input
        where
        combined :: HasClockReset dom gated sync => Signal dom Bool -> Signal dom Bool
        combined s = descrambler initial poly $ scrambler initial poly s
