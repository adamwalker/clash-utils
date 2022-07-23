module DSP.FIR.HalfBandSpec where

import qualified Clash.Prelude as Clash
import Clash.Prelude (Signal, Vec(..), BitVector, Index, Signed, Unsigned, SFixed, Bit, SNat(..),
                      simulate, simulate_lazy, listToVecTH, KnownNat, pack, unpack, (++#), mealy, mux, bundle, unbundle, 
                      HiddenClockResetEnable, type (+), extend, NFDataX, System, fromList, singleton, sample, regEn)
import Control.Applicative
import Test.Hspec
import Test.QuickCheck hiding (sample)

import Clash.DSP.FIR.Filter
import Clash.DSP.FIR.SemiParallel
import Clash.DSP.FIR.HalfBand

import DSP.FIR.Common hiding (goldenExpect)

spec = describe "Half band filters" $ do
    describe "Decimator" $ do
        specify "Non-symmetric"         $ property prop_halfbandDecim
        specify "Symmetric"             $ property prop_halfbandDecimSymm
        specify "Symmetric multi stage" $ property prop_halfbandDecimSymmMulti

goldenExpect coeffs input
    = sample @System 
    $ goldenFIR coeffs (pure True) (fromList $ 0 : input ++ repeat 0) 

--2 phases
--1 stage in semi parallel filter
--4 coefficients in filter
prop_halfbandDecim :: Vec 16 (Signed 32) -> [Signed 32] -> InfiniteList Bool -> Property
prop_halfbandDecim coeffs input (InfiniteList ens _) = expect === result
    where

    coeffsHB 
        = Clash.init 
        $ Clash.merge coeffs 
        $ Clash.replicate (SNat @7) 0 Clash.++ Clash.singleton 1 Clash.++ Clash.repeat 0

    expect
        = drop 15
        $ take (length input)
        $ stride 1 
        $ drop 1
        $ goldenExpect coeffsHB input

    result 
        = drop 15
        $ take (length input) 
        $ drop 1
        $ map snd . filter fst
        $ sample @System 
        $ system (halfBandDecimate' (SNat @7) id filter') input ens

    filter'
        :: forall dom. HiddenClockResetEnable dom 
        => Signal dom (Signed 32) 
        -> Signal dom Bool 
        -> Signal dom (Signed 32) 
        -> (Signal dom Bool, Signal dom (Signed 32), Signal dom Bool)
    filter' = semiParallelFIRSystolic (const macRealReal) (SNat @0) (singleton coeffs) 

--2 phases
--1 stage in semi parallel filter
--4 coefficients in filter
prop_halfbandDecimSymm :: Vec 8 (Signed 32) -> Signed 32 -> [Signed 32] -> InfiniteList Bool -> Property
prop_halfbandDecimSymm coeffs midCoeff input (InfiniteList ens _) = expect === result
    where

    combined = coeffs :< midCoeff
    coeffsHB 
        = Clash.init 
        $ Clash.merge (combined Clash.++ Clash.reverse combined)
        $ Clash.replicate (SNat @7) 0 Clash.++ Clash.singleton 1 Clash.++ Clash.repeat 0

    expect
        = drop 17
        $ take (length input)
        $ stride 1 
        $ drop 1
        $ goldenExpect coeffsHB input

    result 
        = drop 17
        $ take (length input) 
        $ drop 1
        $ map snd . filter fst
        $ sample @System 
        $ system (halfBandDecimate' (SNat @7) id filter') input ens

    filter'
        :: forall dom. HiddenClockResetEnable dom 
        => Signal dom (Signed 32) 
        -> Signal dom Bool 
        -> Signal dom (Signed 32) 
        -> (Signal dom Bool, Signal dom (Signed 32), Signal dom Bool)
    filter' = semiParallelFIRSystolicSymmetric (const macPreAddRealReal) (evenSymmAccum (SNat @0) (\x y -> (x + y) * midCoeff)) (SNat @0) (singleton coeffs) 

--2 phases
--2 stages in semi parallel filter
--4 coefficients in filter
prop_halfbandDecimSymmMulti :: Vec 2 (Vec 8 (Signed 32)) -> [Signed 32] -> InfiniteList Bool -> Property
prop_halfbandDecimSymmMulti coeffs input (InfiniteList ens _) = expect === result
    where

    combined = Clash.concat coeffs 
    coeffsHB 
        = Clash.init 
        $ Clash.merge (combined Clash.++ Clash.reverse combined)
        $ Clash.replicate (SNat @15) 0 Clash.++ Clash.singleton 1 Clash.++ Clash.repeat 0

    expect
        = drop 33
        $ take (length input)
        $ stride 1 
        $ drop 1
        $ goldenExpect coeffsHB input

    result 
        = drop 33
        $ take (length input) 
        $ drop 1
        $ map snd . filter fst
        $ sample @System 
        $ system (halfBandDecimate (const macPreAddRealReal) (SNat @0) id coeffs) input ens

