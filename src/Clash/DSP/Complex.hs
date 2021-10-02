{-# LANGUAGE UndecidableInstances #-} --For the BitPack instance
module Clash.DSP.Complex where

import Clash.Prelude

import GHC.Generics
import Test.QuickCheck

import qualified Data.Complex as C

{-| I defined my own complex type so that I can write a Num instance without the RealFloat constraint. TODO: think about whether this is really a good idea. -}
data Complex a = a :+ a deriving (Show, Lift, Generic, ShowX, NFDataX, Functor, Foldable, Traversable, Eq, Ord)

deriving instance (BitPack a, KnownNat (BitSize a)) => BitPack (Complex a)

instance Num a => Num (Complex a) where
    (a :+ b) + (c :+ d) = (a + c) :+ (b + d)
    (a :+ b) - (c :+ d) = (a - c) :+ (b - d)
    (a :+ b) * (c :+ d) = (a * c - b * d) :+ (a * d + b * c)
    fromInteger x       = (fromInteger x :+ 0)

instance Applicative Complex where
    pure a = a :+ a
    f :+ g <*> a :+ b = f a :+ g b

instance Arbitrary a => Arbitrary (Complex a)
    where
    arbitrary = liftA2 (:+) arbitrary arbitrary

realPart :: Complex a -> a
realPart (x :+ _) = x

imagPart :: Complex a -> a
imagPart (_ :+ y) = y

fromComplex (a C.:+ b) = a :+ b
toComplex   (a :+ b)   = a C.:+ b

conjugate :: Num a => Complex a -> Complex a
conjugate (x :+ y) = x :+ (-y)

cMul 
    :: ExtendingNum a b 
    => ExtendingNum (MResult a b) (MResult a b)
    => Complex a
    -> Complex b
    -> Complex (AResult (MResult a b) (MResult a b))
cMul (xr :+ xi) (yr :+ yi) 
    =  ((xr `mul` yr) `sub` (xi `mul` yi)) 
    :+ ((xr `mul` yi) `add` (xi `mul` yr))

cMul3
    :: ExtendingNum b b
    => ExtendingNum a a
    => ExtendingNum a (AResult b b)
    => ExtendingNum b (AResult a a)
    => ExtendingNum (MResult b (AResult a a)) (MResult a (AResult b b))
    => Complex a
    -> Complex b
    -> Complex (AResult (MResult b (AResult a a)) (MResult a (AResult b b)))
cMul3 (xr :+ xi) (yr :+ yi) 
    =  (k1 `sub` k3)
    :+ (k1 `add` k2)
    where
    k1 = yr `mul` (xr `add` xi)
    k2 = xr `mul` (yi `sub` yr)
    k3 = xi `mul` (yr `add` yi)

cMulPipe
    :: HiddenClockResetEnable dom
    => ExtendingNum a b 
    => ExtendingNum (MResult a b) (MResult a b)
    => NFDataX (MResult a b)
    => NFDataX (AResult (MResult a b) (MResult a b))
    => Signal dom (Complex a)
    -> Signal dom (Complex b)
    -> Signal dom (Complex (AResult (MResult a b) (MResult a b)))
cMulPipe x y 
    = liftA2 (:+) rr ri
    where
    --Products
    xryr = register (errorX "initial xryr") $ liftA2 mul (realPart <$> x) (realPart <$> y)
    xiyi = register (errorX "initial xiyi") $ liftA2 mul (imagPart <$> x) (imagPart <$> y)
    xryi = register (errorX "initial xryi") $ liftA2 mul (realPart <$> x) (imagPart <$> y)
    xiyr = register (errorX "initial xiyr") $ liftA2 mul (imagPart <$> x) (realPart <$> y)
    --Sums
    rr   = register (errorX "initial rr")   $ liftA2 sub xryr xiyi
    ri   = register (errorX "initial ri")   $ liftA2 add xryi xiyr

cMul3Pipe
    :: HiddenClockResetEnable dom
    => ExtendingNum b b
    => ExtendingNum a a
    => ExtendingNum a (AResult b b)
    => ExtendingNum b (AResult a a)
    => ExtendingNum (MResult b (AResult a a)) (MResult a (AResult b b))
    => NFDataX (AResult a a) 
    => NFDataX (AResult b b) 
    => NFDataX (MResult b (AResult a a))
    => NFDataX (MResult a (AResult b b))
    => NFDataX (AResult (MResult b (AResult a a)) (MResult a (AResult b b)))
    => NFDataX a
    => NFDataX b
    => Signal dom (Complex a)
    -> Signal dom (Complex b)
    -> Signal dom (Complex (AResult (MResult b (AResult a a)) (MResult a (AResult b b))))
cMul3Pipe x y
    =  liftA2 (:+) rr ri
    where
    --Sums
    xSum    = register (errorX "initial xSum")    $ liftA2 add (realPart <$> x) (imagPart <$> x)
    yDiff   = register (errorX "initial yDiff")   $ liftA2 sub (imagPart <$> y) (realPart <$> y)
    ySum    = register (errorX "initial ySum")    $ liftA2 add (realPart <$> y) (imagPart <$> y)
    --Delays
    xD      = register (errorX "initial xD") x
    yD      = register (errorX "initial yD") y
    --Products
    yrXSum  = register (errorX "initial yrXSum")  $ liftA2 mul (realPart <$> yD) xSum
    xrYDiff = register (errorX "initial xrYDiff") $ liftA2 mul (realPart <$> xD) yDiff
    xiYSum  = register (errorX "initial xiYSum")  $ liftA2 mul (imagPart <$> xD) ySum
    --Sums
    rr      = register (errorX "initial rr")      $ liftA2 sub yrXSum xiYSum
    ri      = register (errorX "initial ri")      $ liftA2 add yrXSum xrYDiff

