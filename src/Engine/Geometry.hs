{-# LANGUAGE LiberalTypeSynonyms #-}
module Engine.Geometry where

import Engine.Functor
import Control.Applicative
import Data.Foldable
import Engine.Range
import Engine.Vector
import Prelude hiding (foldl1)

type U    a = Vector1 a
type UV   a = Vector2 a
type UVW  a = Vector3 a
type UVWT a = Vector4 a

type Vertex2 a = UV   a
type Vertex3 a = UVW  a
type Vertex4 a = UVWT a

type Normal2 a = UV   a
type Normal3 a = UVW  a
type Normal4 a = UVWT a

getX, getY, getZ, getU, getV, getW, getT :: Vector f => f a -> a
setX, setY, setZ, setU, setV, setW, setT :: Vector f => a -> f a -> f a

getX = select 0
getY = select 1
getZ = select 2

getU = select 0
getV = select 1
getW = select 2
getT = select 3

setX v = update 0 v
setY v = update 1 v
setZ v = update 2 v

setU v = update 0 v
setV v = update 1 v
setW v = update 2 v
setT v = update 3 v

type Point f a = f a
type Line  f a = Range (Point f a)
type Rect  f a = Range (Point f a)

-- 2-Dimensional geometry.

type Point2 a = Point UV a
type Line2  a = Line  UV a
type Rect2  a = Rect  UV a

type Point2i = Point2 Int
type Line2i  = Line2  Int
type Rect2i  = Rect2  Int

-- 3-Dimensional geometry.

type Point3 a = Point UVW a
type Line3  a = Line  UVW a
type Rect3  a = Rect  UVW a

type Point3i = Point3 Int
type Line3i  = Line3  Int
type Rect3i  = Rect3  Int

-- Dimension independent computations.

contained :: (Foldable f, Functor f, Applicative f, Ord a) => Point f a -> Rect f a -> Bool
contained p r = foldl1 (&&) (inRange <$> p <*> flipRange r)

distance :: (Floating a, Foldable f, Applicative f) => Range (f a) -> a
distance (a :~: b) = sqrt $ foldl1 (+) $ fmap (^(2::Int)) $ (-) <$> a <*> b

surface2 :: (Num a, Foldable f, Applicative f) => Range (f a) -> a
surface2 (a :~: b) = foldl1 (*) $ (-) <$> a <*> b

