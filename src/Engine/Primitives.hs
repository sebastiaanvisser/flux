module Engine.Primitives where

import Control.Applicative
import Engine.Geometry
import Engine.Interpolation
import Engine.Projection
import Engine.Vector
import Engine.Range
import Engine.Functor
import Engine.Transform

--------[ primitive UV surfaces ]----------------------------------------------

planeUV :: Fractional a => UV a -> Proj_UV_Vertex3 a a
planeUV s = lift3Dc 0 . scale s . trans (pure (-0.5))

tubeUV
  :: (Floating a, Ipol a, Scalar a)
  => a -> Range a -> Proj_UV_Vertex3 a a
tubeUV h r (V2 u v) =
    scale (V3 1 1 h)
  $ trans (V3 0 0 (-0.5))
  $ lift3Dc ((0 :~: 1) ..< v)
  $ circular1_UV (r ..< v) u

torusUV :: Floating a => a -> a -> Proj_UV_Vertex3 a a
torusUV r0 r1 (V2 u v) =
  let V2 u' v' = circular1_UV r1 v in
  lift3Dc v' $ circular1_UV (r0 + u') u

spiralUV r0 r1 h k (V2 u v) =
  let V2 u' v' = circular1_UV (r1 ..< u) v in
  lift3Dc ((h * u) + v') $ circular_UV k ((r0 ..< u) + u') u

circular_UV :: Floating a => a -> a -> a -> UV a
circular_UV k r t = V2 (r * cos (k*t*pi*2)) (r * sin (k*t*pi*2))

circular1_UV :: Floating a => a -> a -> UV a
circular1_UV = circular_UV 1

