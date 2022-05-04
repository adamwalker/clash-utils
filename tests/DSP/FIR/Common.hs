module DSP.FIR.Common where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable, type (+), extend, NFDataX, System, fromList, singleton, sample, regEn)
import Control.Applicative
import Test.Hspec
import Test.QuickCheck hiding (sample)

import Clash.DSP.FIR.Filter
import Clash.DSP.FIR.SemiParallel
import Clash.DSP.FIR.Polyphase

--Golden model
goldenFIR 
    :: (HiddenClockResetEnable dom, Num a, KnownNat n, NFDataX a)
    => Vec (n + 1) a
    -> Signal dom Bool
    -> Signal dom a
    -> Signal dom a
goldenFIR = fir (*) (+)

goldenExpect coeffs input
    = take (length input) 
    $ sample @System 
    $ goldenFIR coeffs (pure True) (fromList $ 0 : input ++ repeat 0) 

--Dummy test MAC units
macRealReal c i a = c * i + a

macPreAddRealReal c x y a = c * (x + y) + a

macRealRealPipelined en c i a = regEn 0 en (c * i) + a

macPreAddRealRealPipelined en c x y a = regEn 0 en (regEn 0 en c * regEn 0 en (x + y)) + a

liftA4 f w x y z = f <$> w <*> x <*> y <*> z

--A type for filters that can take an input and produce an output every cycle
type FilterNoReady dom a
    =  Signal dom Bool   -- ^ Input valid
    -> Signal dom a      -- ^ Sample
    -> Signal dom a      -- ^ (Output valid, output data)

streamList 
    :: (HiddenClockResetEnable dom, NFDataX a)
    => [a]
    -> [Bool]
    -> Signal dom Bool
    -> (Signal dom Bool, Signal dom a)
streamList samples enables = unbundle . mealy step (samples, enables)
    where
    step :: ([a], [Bool]) -> Bool -> (([a], [Bool]), (Bool, a))
    step (l@(x:xs), es@(True:_)) False = ((l, es),  (True, x))
    step (l@(x:xs), (e:es))      False = ((l, es),  (e, x))
    step (l@(x:xs), (False:es))  True  = ((l, es),  (False, x))
    step (  (x:xs), (True:es))   True  = ((xs, es), (True, x))
    step ([],       _)           _     = error "Data list empty"
    step (_,        [])          _     = error "Enable list empty"

systemNoReady
    :: forall dom a
    .  (HiddenClockResetEnable dom, NFDataX a, Num a)
    => FilterNoReady dom a
    -> [a]
    -> [Bool]
    -> Signal dom (Bool, a)
systemNoReady filter samples ens = bundle (valids, out)
    where
    (valids, sampleStream) = streamList (samples ++ repeat 0) ens (pure True)
    out                    = filter valids sampleStream

system 
    :: forall dom a
    .  (HiddenClockResetEnable dom, NFDataX a, Num a)
    => Filter dom a
    -> [a]
    -> [Bool]
    -> Signal dom (Bool, a)
system filter samples ens = bundle (vld, out)
    where
    (valids, sampleStream) = streamList (samples ++ repeat 0) ens ready
    (vld, out, ready)      = filter valids sampleStream

stride :: Int -> [a] -> [a]
stride s = go
    where
    go (x:xs) = x : go (Prelude.drop s xs)
    go _      = []

