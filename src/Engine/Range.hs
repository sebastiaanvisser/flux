module Engine.Range where

import Control.Applicative
import Engine.Math

data Range a = a :~: a
  deriving Show

instance Functor Range where
  fmap f (a :~: b) = (f a :~: f b)

sharp :: Almost a => a -> Range a
sharp a = almost a :~: just a

-- distance :: Num a => Range a -> a
-- distance (a :~: b) = b - a

inRange :: Ord a => a -> Range a -> Bool
inRange x (a :~: b) = x >= a && x <= b

flipRange :: Applicative f => Range (f a) -> f (Range a)
flipRange (a :~: b) = (:~:) <$> a <*> b

