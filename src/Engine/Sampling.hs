module Engine.Sampling where

import Control.Applicative
import Engine.Math
import Engine.Range
import Engine.Functor
import Engine.Geometry
import Engine.Interpolation

-- Samplers types.

type Samples a = [a]

type Sampler_U   a = U   (Samples a)
type Sampler_UV  a = UV  (Samples a)
type Sampler_UVW a = UVW (Samples a)

-- Sampler helpers.

linear' :: (Fractional b, Enum b, Ipol a, Scalar b) => Range a -> b -> [a]
linear' (a :~: b) s = [a :~: b ..< (t/s) | t <- [0..s]]

linear1' :: (Fractional b, Enum b, Ipol a, Scalar b, Num a) => b -> [a]
linear1' = linear' (0 :~: 1)

subdivide' :: (Scalar b, Enum b, Fractional b, Ipol a) => b -> [a] -> [a]
subdivide' s x = head x : cat grd x
  where cat f = concat . (\a -> zipWith f (init a) (tail a))
        grd a b = tail $ linear' (a :~: b) s

sharpen' :: Almost a => Samples a -> Samples a
sharpen' []     = []
sharpen' (x:xs) = x : init (concatMap (\n -> let a :~: b = sharp n in [a, b]) xs)

-- Samplers.

linear :: (Fractional a, Enum a, Scalar a, Ipol t, Functor f) => Range t -> f a -> f (Samples t)
linear r s = linear' r <$> s

linear1 :: (Fractional a, Enum a, Ipol b, Scalar a, Num b, Functor f) => f a -> f [b]
linear1 s = linear1' <$> s

sharpen :: (Almost t, Functor f) => f (Samples t) -> f (Samples t)
sharpen = fmap sharpen'

subdivide :: (Integral a, Scalar a, Fractional a, Ipol t, Applicative f) => f a -> f (Samples t) -> f (Samples t)
subdivide a = (subdivide' <$> a <*>)

-- Conversion from samplers to segmentations.

type Segmentation a = Samples (Range a)

segmented :: Samples a -> Segmentation a
segmented a = zipWith (:~:) (init a) (tail a)

