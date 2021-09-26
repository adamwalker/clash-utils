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

