{-# LANGUAGE DeriveLift, DeriveGeneric, DeriveAnyClass, DeriveTraversable, StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-} --For the BitPack instance
module Clash.DSP.Complex where

import Clash.Prelude

import GHC.Generics

import qualified Data.Complex as C

{-| I defined my own complex type so that I can write a Num instance without the RealFloat constraint. TODO: think about whether this is really a good idea. -}
data Complex a = a :+ a deriving (Show, Lift, Generic, ShowX, NFDataX, Functor, Foldable, Traversable)

deriving instance (BitPack a, KnownNat (BitSize a)) => BitPack (Complex a)

instance Num a => Num (Complex a) where
    (a :+ b) + (c :+ d) = (a + c) :+ (b + d)
    (a :+ b) - (c :+ d) = (a - c) :+ (b - d)
    (a :+ b) * (c :+ d) = (a * c - b * d) :+ (a * d + b * c)
    fromInteger x       = (fromInteger x :+ 0)

instance Applicative Complex where
    pure a = a :+ a
    f :+ g <*> a :+ b = f a :+ g b

realPart :: Complex a -> a
realPart (x :+ _) = x

imagPart :: Complex a -> a
imagPart (_ :+ y) = y

fromComplex (a C.:+ b) = a :+ b
toComplex   (a :+ b)   = a C.:+ b

conjugate :: Num a => Complex a -> Complex a
conjugate (x :+ y) = x :+ (-y)
