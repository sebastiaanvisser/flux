{-# LANGUAGE FlexibleInstances #-}
module Engine.Functor where

import Data.Word
import Control.Applicative
import Data.Foldable
import Prelude hiding (foldl1, foldr1)

--------[ second arity functor ]-----------------------------------------------

class Functor2 f where
  fmap2 :: (a -> c) -> (b -> d) -> f a b -> f c d

instance Functor2 (,) where
  fmap2 f g (a, b) = (f a, g b)

--------[ third arity functor ]------------------------------------------------

class Functor3 g where
  fmap3 :: (a -> d) -> (b -> e) -> (c -> f) -> g a b c -> g d e f

instance Functor3 (,,) where
  fmap3 f g h (a, b, c) = (f a, g b, h c)

--------[ fourth arity functor ]-----------------------------------------------

class Functor4 k where
  fmap4 :: (a -> e) -> (b -> f) -> (c -> g) -> (d -> h) -> k a b c d -> k e f g h

instance Functor4 (,,,) where
  fmap4 f g h i (a, b, c, d) = (f a, g b, h c, i d)

--------[ applicative extensions ]---------------------------------------------

fzip :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
fzip f a b = f <$> a <*> b

--------[ foldable extensions ]------------------------------------------------

first :: Foldable f => f a -> a
first = foldr1 const

flast :: Foldable f => f a -> a
flast = foldr1 (flip const)

-------------------------------------------------------------------------------

-- The amount of elements in a container.
class Size f where
  size :: f a -> Int

-- A scalar representation of a value.
class Scalar a where
  scalar :: a -> Float

instance Scalar Float where
  scalar = id

instance Scalar Word8 where
  scalar = fromIntegral

-- For foldable functors for which we know the size, we can compute a scalar
-- version by taking the average.
instance (Size f, Functor f, Foldable f, Scalar a) => Scalar (f a) where
  scalar f = (/fromIntegral (size f)) $ foldl1 (+) $ fmap scalar f

-------------------------------------------------------------------------------

-- Rotate elements in a container.
class Cyclic f where
  left  :: f a -> f a
  right :: f a -> f a

iterative :: Integer -> (a -> a) -> a -> a
iterative 0 _ = id
iterative n f = f . iterative (n-1) f

select :: (Foldable f, Cyclic f) => Integer -> f a -> a
select n = first . iterative n left

class Cons f where
  cons :: a -> f a -> f a

update :: (Cyclic f, Cons f, Applicative f) => Integer -> b -> f b -> f b
update n v = (<*>) (iterative n right $ cons (const v) $ pure id)

