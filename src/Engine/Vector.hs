{-# LANGUAGE UndecidableInstances, RankNTypes #-}

module Engine.Vector where

import Control.Applicative
import Data.Foldable
import Data.Traversable
import Engine.Functor
import Foreign
import Prelude hiding (foldl1, foldr1, foldr)

-- Dimension indenpendent vectors.

class (
    Applicative f
  , Cyclic      f
  , Cons        f
  , Foldable    f
  , Traversable f
  , Functor     f
  , Size        f
  ) => Vector f where

magnitude :: (Vector f, Floating a) => f a -> a
magnitude = sqrt . foldl1 (+) . fmap (^(2::Int))

normalize :: (Vector f, Floating a) => f a -> f a
normalize v = fmap (/magnitude v) v

infixl 7  *., .*.
infixl 6  +., -.

(-.) :: (Num a, Vector f) => f a -> f a -> f a
(-.) = fzip (-)

(+.) :: (Num a, Vector f) => f a -> f a -> f a
(+.) = fzip (+)

(*.) :: (Num a, Vector f) => a -> f a -> f a
(*.) c = fmap (*c)

(.*.) :: (Num a, Vector f) => f a -> f a -> a
(.*.) a b = foldl1 (+) $ fzip (*) a b

unit :: (Applicative f, Num a) => f a
unit = pure 1

angle :: (Vector f, Floating a) => f a -> f a -> a
angle a b = acos (normalize a .*. normalize b)

angle1 :: (Vector f, Floating a) => f a -> f a -> a
angle1 a b = abs (1 - angle a b / pi)

-- 1-Dimensional vector.

data Vector1 a = V1 !a
  deriving (Show, Eq, Ord)

instance Storable a => Storable (Vector1 a) where
  sizeOf v      = sizeOf (first v)
  alignment _   = 4
  peek p        = let f = peek (castPtr p) in V1 <$> f
  poke p (V1 a) = let f = poke (castPtr p) in        f a

instance Functor Vector1 where
  fmap f (V1 a) = V1 (f a)

instance Applicative Vector1 where
  pure a = V1 a
  V1 f <*> V1 a = V1 (f a)

instance Foldable Vector1 where
  foldr f z (V1 a) = f a z

instance Traversable Vector1 where
  sequenceA (V1 a) = V1 <$> a

instance Cyclic Vector1 where
  left  = id
  right = id

instance Cons Vector1 where
  cons a = const (V1 a)

instance Size Vector1 where
  size _ = 1

instance Vector Vector1 where

-- 2-Dimensional vector.

data Vector2 a = V2 !a !a
  deriving (Show, Eq, Ord)

instance Storable a => Storable (Vector2 a) where
  sizeOf v        = sizeOf (first v) * 2
  alignment _     = 4
  peek p          = let f = peekElemOff (castPtr p) in V2 <$> f 0  <*> f 1
  poke p (V2 a b) = let f = pokeElemOff (castPtr p) in        f 0 a >> f 1 b

instance Functor Vector2 where
  fmap f (V2 a b) = V2 (f a) (f b)

instance Applicative Vector2 where
  pure a = V2 a a
  V2 f g <*> V2 a b = V2 (f a) (g b)

instance Foldable Vector2 where
  foldr f z (V2 a b) = f a $ f b z

instance Traversable Vector2 where
  sequenceA (V2 a b) = V2 <$> a <*> b

instance Cyclic Vector2 where
  left  (V2 a b) = V2 b a
  right (V2 a b) = V2 b a

instance Cons Vector2 where
  cons a (V2 _ b) = V2 a b

instance Size Vector2 where
  size _ = 2

instance Vector Vector2 where

-- 3-Dimensional vector.

data Vector3 a = V3 !a !a !a
  deriving (Show, Eq, Ord)

instance Storable a => Storable (Vector3 a) where
  sizeOf v          = sizeOf (first v) * 3
  alignment _       = 4
  peek p            = let f = peekElemOff (castPtr p) in V3 <$> f 0  <*> f 1  <*> f 2
  poke p (V3 a b c) = let f = pokeElemOff (castPtr p) in        f 0 a >> f 1 b >> f 2 c

instance Functor Vector3 where
  fmap f (V3 a b c) = V3 (f a) (f b) (f c)

instance Applicative Vector3 where
  pure a = V3 a a a
  V3 f g h <*> V3 a b c = V3 (f a) (g b) (h c)

instance Foldable Vector3 where
  foldr f z (V3 a b c) = f a $ f b $ f c z

instance Traversable Vector3 where
  sequenceA (V3 a b c) = V3 <$> a <*> b <*> c

instance Cyclic Vector3 where
  left  (V3 a b c) = V3 b c a
  right (V3 a b c) = V3 c a b

instance Cons Vector3 where
  cons a (V3 _ b c) = V3 a b c

instance Size Vector3 where
  size _ = 3

instance Vector Vector3 where

lift3Da, lift3Db, lift3Dc :: a -> Vector2 a -> Vector3 a
lift3Da a (V2 b c) = V3 a b c
lift3Db b (V2 a c) = V3 a b c
lift3Dc c (V2 a b) = V3 a b c

vectorI, vectorJ, vectorK :: Num a => Vector3 a
vectorI = V3 1 0 0
vectorJ = V3 0 1 0
vectorK = V3 0 0 1

-- todo generalize:
cross3 :: Num a => Vector3 a -> Vector3 a -> Vector3 a
cross3 (V3 a b c) (V3 d e f) = V3 (b*f - c*e) (c*d - a*f) (a*e - b*d)

-- 4-Dimensional vector.

data Vector4 a = V4 !a !a !a !a
  deriving (Show, Eq, Ord)

instance Storable a => Storable (Vector4 a) where
  sizeOf v            = sizeOf (first v) * 4
  alignment _         = 4
  peek p              = let f = peekElemOff (castPtr p) in V4 <$> f 0  <*> f 1  <*> f 2  <*> f 3
  poke p (V4 a b c d) = let f = pokeElemOff (castPtr p) in        f 0 a >> f 1 b >> f 2 c >> f 3 d

instance Functor Vector4 where
  fmap f (V4 a b c d) = V4 (f a) (f b) (f c) (f d)

instance Foldable Vector4 where
  foldr f z (V4 a b c d) = f a $ f b $ f c $ f d z

instance Applicative Vector4 where
  pure a = V4 a a a a
  V4 f g h i <*> V4 a b c d = V4 (f a) (g b) (h c) (i d)

instance Cyclic Vector4 where
  left   (V4 a b c d) = V4 b c d a
  right  (V4 a b c d) = V4 d a b c

instance Cons Vector4 where
  cons a (V4 _ b c d) = V4 a b c d

instance Size Vector4 where
  size _ = 4

instance Traversable Vector4 where
  sequenceA (V4 a b c d) = V4 <$> a <*> b <*> c <*> d

instance Vector Vector4 where

lift4Da, lift4Db, lift4Dc, lift4Dd :: a -> Vector3 a -> Vector4 a
lift4Da a (V3 b c d) = V4 a b c d
lift4Db b (V3 a c d) = V4 a b c d
lift4Dc c (V3 a b d) = V4 a b c d
lift4Dd d (V3 a b c) = V4 a b c d

-------------------------------------------------------------------------------

newtype Matrix a b c = Matrix { unM :: a (b c) }
  deriving (Eq, Ord, Show)

withMatrix :: (f (g a) -> u (v b)) -> Matrix f g a -> Matrix u v b
withMatrix f (Matrix m) = Matrix (f m)

instance (Vector f, Vector g) => Functor (Matrix f g) where
  fmap = withMatrix . fmap . fmap

instance (Vector f, Vector g) => Foldable (Matrix f g) where
  foldr f z = foldr (flip $ foldr f) z . unM

instance (Vector f, Vector g) => Applicative (Matrix f g) where
  pure = Matrix . pure . pure
  Matrix a <*> Matrix b = Matrix ((<*>) <$> a <*> b)

-- instance Cyclic Vector4 where
--   left   (V4 a b c d) = V4 b c d a
--   right  (V4 a b c d) = V4 d a b c

-- instance Cons Vector4 where
--   cons a (V4 _ b c d) = V4 a b c d

-- instance Size Vector4 where
--   size _ = 4

-- instance Vector Vector4 where

type Matrix2x2 a = Matrix Vector2 Vector2 a
type Matrix3x3 a = Matrix Vector3 Vector3 a
type Matrix4x4 a = Matrix Vector4 Vector4 a

transpose :: (Traversable f, Applicative g) => Matrix f g a -> Matrix g f a
transpose = withMatrix sequenceA

-- Matrix product.
prod
  :: (Vector f, Vector g, Vector h, Num a)
  => Matrix f g a -> Matrix g h a -> Matrix f h a
prod (Matrix a) (Matrix b) = Matrix (fmap f a)
  where f row = fmap (\col -> foldr (+) 0 ((*) <$> row <*> col)) (sequenceA b)

