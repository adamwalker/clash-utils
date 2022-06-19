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

instance Default a => Default (Complex a)
    where
    def = def :+ def

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
    => Signal dom Bool
    -> Signal dom (Complex a)
    -> Signal dom (Complex b)
    -> Signal dom (Complex (AResult (MResult a b) (MResult a b)))
cMulPipe en x y 
    = liftA2 (:+) rr ri
    where
    --Products
    xryr = delayEn (errorX "initial xryr") en $ liftA2 mul (realPart <$> x) (realPart <$> y)
    xiyi = delayEn (errorX "initial xiyi") en $ liftA2 mul (imagPart <$> x) (imagPart <$> y)
    xryi = delayEn (errorX "initial xryi") en $ liftA2 mul (realPart <$> x) (imagPart <$> y)
    xiyr = delayEn (errorX "initial xiyr") en $ liftA2 mul (imagPart <$> x) (realPart <$> y)
    --Sums
    rr   = delayEn (errorX "initial rr")   en $ liftA2 sub xryr xiyi
    ri   = delayEn (errorX "initial ri")   en $ liftA2 add xryi xiyr

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
    => Signal dom Bool
    -> Signal dom (Complex a)
    -> Signal dom (Complex b)
    -> Signal dom (Complex (AResult (MResult b (AResult a a)) (MResult a (AResult b b))))
cMul3Pipe en x y
    =  liftA2 (:+) rr ri
    where
    --Sums
    xSum    = delayEn (errorX "initial xSum")    en $ liftA2 add (realPart <$> x) (imagPart <$> x)
    yDiff   = delayEn (errorX "initial yDiff")   en $ liftA2 sub (imagPart <$> y) (realPart <$> y)
    ySum    = delayEn (errorX "initial ySum")    en $ liftA2 add (realPart <$> y) (imagPart <$> y)
    --Delays
    xD      = delayEn (errorX "initial xD") en x
    yD      = delayEn (errorX "initial yD") en y
    --Products
    yrXSum  = delayEn (errorX "initial yrXSum")  en $ liftA2 mul (realPart <$> yD) xSum
    xrYDiff = delayEn (errorX "initial xrYDiff") en $ liftA2 mul (realPart <$> xD) yDiff
    xiYSum  = delayEn (errorX "initial xiYSum")  en $ liftA2 mul (imagPart <$> xD) ySum
    --Sums
    rr      = delayEn (errorX "initial rr")      en $ liftA2 sub yrXSum xiYSum
    ri      = delayEn (errorX "initial ri")      en $ liftA2 add yrXSum xrYDiff

