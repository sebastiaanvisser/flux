module Engine.Normal where

import Control.Applicative
import Engine.Projection
import Engine.Vector
import Engine.Functor
import Engine.Geometry

type Tangent3 a = Vector3 (Vector3 a)

getS, getT, getN :: Vector f => f a -> a

getS = select 0
getT = select 1
getN = select 2

type Proj_Tangent       a b = Proj a b
type Proj_Tangent3      a b = Proj a (Tangent3 b)
type Proj_UV_Tangent3   a b = Proj_Tangent3 (UV   a) b
type Proj_UVW_Tangent3  a b = Proj_Tangent3 (UVW  a) b
type Proj_UVWT_Tangent3 a b = Proj_Tangent3 (UVWT a) b

tangent3 
  :: (Floating a, Fractional t)
  => Proj_UV_Vertex3 t a
  -> Proj_UV_Tangent3 t a
tangent3 vp p@(V2 u v) =
  let o = vp p
      s = normalize (vp (V2 (u+0.001) v) -. o)
      t = normalize (vp (V2 u (v+0.001)) -. o) in
    V3 s t (s `cross3` t)

type Proj_Normal       a b = Proj a b
type Proj_Normal3      a b = Proj a (Normal3 b)
type Proj_UV_Normal3   a b = Proj_Normal3 (UV   a) b
type Proj_UVW_Normal3  a b = Proj_Normal3 (UVW  a) b
type Proj_UVWT_Normal3 a b = Proj_Normal3 (UVWT a) b

normal3 
  :: (Floating a, Fractional t)
  => Proj_UV_Vertex3 t a
  -> Proj_UV_Normal3 t a
normal3 p = getN . tangent3 p


derivative_UV vp (V2 u v) =
  let a = normal3 vp (V2 u v)
  in V3
    (angle1 a (V3 1 0 0))
    (angle1 a (V3 0 1 0))
    (angle1 a (V3 0 0 1))

flux_UV vp (V2 u v) =
  let a = normal3 vp (V2 u v)
      b = normal3 vp (V2 (u+0.1) v)
      c = normal3 vp (V2 u (v+0.1))
  in V2 (angle1 a b) (angle1 a c)


