module Container.FIFOSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable, System)
import GHC.TypeLits
import Test.Hspec
import Test.QuickCheck

import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Maybe
import Clash.Container.FIFO
import Clash.Stream.Test

--FIFO
--This software model should behave identically to the hardare FIFO
spec = describe "FIFO" $ do
    specify "data streams through FIFO"                  $ property $ propStreamIdentity fifoStream
    specify "equivalent to software model"               $ property prop_FIFOs
    specify "maybe version equivalent to software model" $ property prop_FIFOMaybe

fifoStream
    :: forall a dom
    .  (HiddenClockResetEnable dom)
    => Signal dom Bool                                    -- ^ Input valid
    -> Signal dom Int                                     -- ^ Data
    -> Signal dom Bool                                    -- ^ Downstream ready
    -> (Signal dom Bool, Signal dom Int, Signal dom Bool) -- ^ (Output valid, output data, ready)
fifoStream valid dat ready = (not <$> empty, dat', not <$> full)
    where
    (dat', empty, full, _) = blockRamFIFO (SNat @8) ready dat valid

fifoStep :: Int -> (Int, Seq a) -> (Bool, a, Bool) -> ((Int, Seq a), (a, Bool, Bool, Int))
fifoStep size (lastSize, storage) (readEn, writeValue, writeEn) = ((nextSize, storage''), (extract storage, empty, full, nItems)) 
    where
    full             = Seq.length storage == size - 1
    nextSize         = Seq.length storage'
    empty            = lastSize == 0
    --Do the read operation before the write
    storage'
        | readEn && not empty = Seq.drop 1 storage
        | otherwise           = storage 
    storage''
        | writeEn && not full = storage' |> writeValue
        | otherwise           = storage'
    extract :: Seq a -> a
    extract s = case Seq.viewl s of
        (x Seq.:< rest) -> x
    nItems = Seq.length storage

compareOutputs :: Eq a => (a, Bool, Bool, Int) -> (a, Bool, Bool, Int) -> Bool
compareOutputs (val1, empty1, full1, size1) (val2, empty2, full2, size2) 
    =  empty1 == empty2 
    && full1  == full2 
    && (empty1 || val1 == val2)
    && size1 == size2

prop_FIFOs :: [(Bool, BitVector 32, Bool)] -> Bool
prop_FIFOs signals = and $ zipWith compareOutputs expect result
    where
    expect = take (length signals) $ simulate_lazy @System (mealy (fifoStep 8) (0, Seq.empty)) signals
    result = take (length signals) $ simulate_lazy @System hackedFIFO signals
    hackedFIFO :: HiddenClockResetEnable dom => Signal dom (Bool, BitVector 32, Bool) -> Signal dom (BitVector 32, Bool, Bool, Int)
    hackedFIFO = bundle . func . unbundle 
        where
        func (x, y, z) = 
            let (dat, empty, full, num) = blockRamFIFO (SNat @3) x y z
            in  (dat, empty, full, fromIntegral <$> num)

prop_FIFOMaybe :: [(Bool, BitVector 32, Bool)] -> Bool
prop_FIFOMaybe signals = Prelude.and $ Prelude.zipWith compareOutputs expect result
    where
    expect = take (length signals) $ simulate_lazy @System (mealy (fifoStep 8) (0, Seq.empty)) signals
    result = take (length signals) $ simulate_lazy @System hackedFIFO signals
    hackedFIFO :: HiddenClockResetEnable dom => Signal dom (Bool, BitVector 32, Bool) -> Signal dom (BitVector 32, Bool, Bool, Int)
    hackedFIFO inputs = bundle $ (fromJust <$> readDataM, (not . isJust) <$> readDataM, full, fromIntegral <$> nItems)
        where
        (readReq, writeData, writeReq) = unbundle inputs
        (readDataM, full, nItems)      = blockRamFIFOMaybe (SNat @3) readReq $ mux writeReq (Just <$> writeData) (pure Nothing)

