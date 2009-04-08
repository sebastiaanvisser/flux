{-# LANGUAGE FlexibleInstances #-}
{-  # LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Engine.Interpolation where

import Control.Applicative
import Engine.Functor
import Engine.Types
import Engine.Range

--------[ interpolation class ]------------------------------------------------

infixl ..<

class Ipol a where
  (..<) :: Scalar b => Range a -> b -> a

--------[ interpolation based utilities ]--------------------------------------

mid :: Ipol a => Range a -> a
mid g = g ..< (0.5 :: Float)

-- grow :: (Ipol a, Fractional a) => a -> Range a -> Range a
-- grow k g = let c = mid g ; d = 0.5 * distance g in (c + d*k) :~: (c - d*k)

split :: Ipol a => Range a -> Tupel (Range a)
split r@(a :~: b) = let m = mid r in (a :~: m, m :~: b)

--------[ interpolation instances ]--------------------------------------------

instance Ipol Float where
  a :~: b ..< s = a + (b-a) * scalar s

-- instance (Ipol a, Ipol b) => Ipol (a, b) where
--   (a, b) :~: (c, d) ..< s = (a :~: c ..< s, b :~: d ..< s)

instance (Applicative f, Ipol a) => Ipol (f a) where
  a :~: b ..< s = fzip (\x y -> x :~: y ..< s) a b

