{-# LANGUAGE KindSignatures, TypeFamilies, UndecidableInstances, RankNTypes #-}
{-| Radix 2 complex-to-complex Cooley-Tukey FFTs. https://en.wikipedia.org/wiki/Cooley%E2%80%93Tukey_FFT_algorithm.
    The FFTs in this module are serial, saving multiplers and routing resources. They operate on and produce two complex numbers at a time. 
-}
module CLaSH.FFTSerial (
    fftSerialDITStep,
    fftSerialDIT,
    fftSerialDIFStep,
    fftSerialDIF
    ) where

import CLaSH.Prelude

import CLaSH.Complex
import CLaSH.FFT(halveTwiddles)
import Data.Singletons.Prelude
import Data.Proxy

fftBase :: Num a => Signal Bool -> Signal (Complex a, Complex a) -> Signal (Complex a, Complex a)
fftBase en = regEn (0, 0) en . fmap func
    where
    func (x, y) = (x + y, x - y)

--Decimation in time
--2^(n + 1) == size of FFT / 2 == number of butterfly input pairs
-- | A step in the serial FFT decimation in time algorithm. Consumes and produces two complex samples per cycle. 
fftSerialDITStep
    :: forall n a. (KnownNat n, Num a)
    => Vec (2 ^ (n + 1)) (Complex a) -- ^ Precomputed twiddle factors
    -> Signal Bool                   -- ^ Input enable signal
    -> Signal (Complex a, Complex a) -- ^ Pair of input samples
    -> Signal (Complex a, Complex a) -- ^ Pair of output samples
fftSerialDITStep twiddles en input = bundle (butterflyHighOutput, butterflyLowOutput)
    where

    counter :: Signal (BitVector (n + 1))
    counter = regEn 0 en (counter + 1)

    (stage' :: Signal (BitVector 1), address' :: Signal (BitVector n)) = unbundle $ split <$> counter

    stage :: Signal Bool
    stage = unpack <$> stage'

    address :: Signal (Unsigned n)
    address = unpack <$> address'

    upperData = mux (not <$> regEn False en stage) (regEn 0 en $ fst <$> input) lowerRamReadResult

    lowerData = mux (not <$> regEn False en stage) lowerRamReadResult (regEn 0 en $ fst <$> input)

    lowerRamReadResult = blockRamPow2 (repeat 0 :: Vec (2 ^ n) (Complex a)) address 
        $ mux en (Just <$> bundle (address, snd <$> input)) (pure Nothing)

    upperRamReadResult = blockRamPow2 (repeat 0 :: Vec (2 ^ n) (Complex a)) (regEn 0 en address)
        $ mux en (Just <$> bundle (regEn 0 en address, upperData)) (pure Nothing)

    --Finally, the butterfly
    butterflyHighInput = upperRamReadResult
    butterflyLowInput  = regEn 0 en lowerData

    twiddle  = (twiddles !!) <$> (regEn 0 en $ regEn 0 en (counter - snatToNum (SNat @ (2 ^ n))))
    twiddled = butterflyLowInput * twiddle

    butterflyHighOutput = butterflyHighInput + twiddled
    butterflyLowOutput  = butterflyHighInput - twiddled 

-- | Example serial FFT decimation in time algorithm. Consumes and produces two complex samples per cycle. Note that both the input and output samples must be supplied in a weird order. See the tests.
fftSerialDIT
    :: forall a. Num a
    => Vec 4 (Complex a)             -- ^ Precomputed twiddle factors
    -> Signal Bool                   -- ^ Input enable signal
    -> Signal (Complex a, Complex a) -- ^ Pair of input samples
    -> Signal (Complex a, Complex a) -- ^ Pair of output samples
fftSerialDIT twiddles en input = 
    fftSerialDITStep twiddles (de . de . de . de $ en) $ 
    fftSerialDITStep cexp2    (de en) $ 
    fftBase en input

    where

    de = register False

    cexp2 :: Vec 2 (Complex a)
    cexp2 = halveTwiddles twiddles

--Decimation in frequency
--2^(n + 1) == size of FFT / 2 == number of butterfly input pairs
-- | A step in the serial FFT decimation in frequency algorithm. Consumes and produces two complex samples per cycle. 
fftSerialDIFStep
    :: forall n a. (KnownNat n, Num a)
    => Vec (2 ^ (n + 1)) (Complex a) -- ^ Precomputed twiddle factors
    -> Signal Bool                   -- ^ Input enable signal
    -> Signal (Complex a, Complex a) -- ^ Pair of input samples
    -> Signal (Complex a, Complex a) -- ^ Pair of output samples
fftSerialDIFStep twiddles en input = bundle (upperRamReadResult, regEn 0 en lowerData)
    where

    --The state
    counter :: Signal (BitVector (n + 1))
    counter = regEn 0 en (counter + 1)

    (stage' :: Signal (BitVector 1), address' :: Signal (BitVector n)) = unbundle $ split <$> counter

    stage :: Signal Bool
    stage = unpack <$> stage'

    address :: Signal (Unsigned n)
    address = unpack <$> address'

    --The butterfly
    butterflyHighOutput          = fmap fst input + fmap snd input
    butterflyLowOutputPreTwiddle = fmap fst input - fmap snd input

    twiddle            = (twiddles !!) <$> counter
    butterflyLowOutput = butterflyLowOutputPreTwiddle * twiddle

    --The FIFOs
    upperData = mux (not <$> regEn False en stage) (regEn 0 en butterflyHighOutput) lowerRamReadResult

    lowerData = mux (not <$> regEn False en stage) lowerRamReadResult (regEn 0 en butterflyHighOutput)

    lowerRamReadResult = blockRamPow2 (repeat 0 :: Vec (2 ^ n) (Complex a)) address 
        $ mux en (Just <$> bundle (address, butterflyLowOutput)) (pure Nothing)

    upperRamReadResult = blockRamPow2 (repeat 0 :: Vec (2 ^ n) (Complex a)) (regEn 0 en address)
        $ mux en (Just <$> bundle (regEn 0 en address, upperData)) (pure Nothing)

-- | Example serial FFT decimation in frequency algorithm. Consumes and produces two complex samples per cycle. Note that both the input and output samples must be supplied in a weird order. See the tests.
fftSerialDIF
    :: forall a. Num a
    => Vec 4 (Complex a)             -- ^ Precomputed twiddle factors
    -> Signal Bool                   -- ^ Input enable signal
    -> Signal (Complex a, Complex a) -- ^ Pair of input samples
    -> Signal (Complex a, Complex a) -- ^ Pair of output samples
fftSerialDIF twiddles en input = 
    fftBase (de . de . de . de . de . de . de $ en) $
    fftSerialDIFStep cexp2    (de . de . de . de $ en) $ 
    fftSerialDIFStep twiddles en input

    where

    de = register False

    cexp2 :: Vec 2 (Complex a)
    cexp2 = halveTwiddles twiddles

dfold' :: forall p k a . KnownNat k
      => Proxy (p :: TyFun Nat * -> *) -- ^ The /motive/
      -> (forall l . (l <= k) => SNat l -> a -> (p @@ l) -> (p @@ (l + 1)))
      -- ^ Function to fold.
      --
      -- __NB__: The @SNat l@ is __not__ the index (see (`!!`)) to the
      -- element /a/. @SNat l@ is the number of elements that occur to the
      -- right of /a/.
      -> (p @@ 0) -- ^ Initial element
      -> Vec k a -- ^ Vector to fold over
      -> (p @@ k)
dfold' = dfold'

data FFTStep (n :: Nat) (a :: *) (f :: TyFun Nat *) :: *
type instance Apply (FFTStep n a) k = (Signal Bool -> Signal (Complex a, Complex a) -> Signal (Complex a, Complex a), Vec (2 ^ (n - k + 1)) (Complex a))

fftGeneral 
    :: forall n a. (KnownNat n, Num a)
    => Vec (2 ^ (n + 1)) (Complex a)
    -> Signal Bool
    -> Signal (Complex a, Complex a)
    -> Signal (Complex a, Complex a)
fftGeneral twiddles = fst $ dfold' (Proxy @ (FFTStep n a)) step base (replicate (SNat @ n) ())
    where
    step :: forall l. (l <= n) => SNat l -> () -> FFTStep n a @@ l -> FFTStep n a @@ (l + 1)
    step SNat _ (stepsToRight, twiddles) = (
            \en inp -> fftSerialDITStep twiddles en $ stepsToRight en inp, 
            halveTwiddles twiddles
        )

    base :: FFTStep n a @@ 0 
    base = (const id, twiddles)

