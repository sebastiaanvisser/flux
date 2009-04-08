module Engine.Transform where

import Control.Applicative
import Data.Foldable
import Engine.Interpolation
import Engine.Functor
import Engine.Math
import Engine.Range
import Prelude hiding (foldl1)

scale :: (Num a, Applicative f) => f a -> f a -> f a
scale m i = (*) <$> m <*> i

scale' :: (Fractional a, Applicative f) => f a -> f a -> f a
scale' m i = (/) <$> i <*> m

flip :: (Num a, Applicative f) => f a -> f a
flip i = pure (*(-1)) <*> i

trans :: (Num a, Applicative f) => f a -> f a -> f a
trans m i = (+) <$> m <*> i

trans' :: (Num a, Applicative f) => f a -> f a -> f a
trans' m i = (-) <$> i <*> m

tile :: (RealFrac a, Applicative f) => f a -> f a -> f a
tile t i = frac <$> ((*) <$> t <*> i)

tile' :: (RealFrac a, Applicative f) => f a -> f a -> f a
tile' t i = frac <$> ((/) <$> i <*> t)

sinoid :: (Size f, Floating a, Applicative f, Foldable f, Functor f) => f a -> f a
sinoid i = average (sin . (2 * pi *) <$> i)

average :: (Size f, Fractional a, Applicative f, Foldable f) => f a -> f a
average i = pure $ (/fromIntegral (size i)) $ foldl1 (+) i

mix :: (Scalar a, Ipol b, Applicative f) => f a -> f b -> f b -> f b
mix m k l = (\a b c -> a :~: b ..< (scalar c :: Float)) <$> k <*> l <*> m

bias :: (Functor f, Fractional a) => f a -> f a
bias = fmap ((+0.5) . (*0.5))

