module Engine.Modifiers where

import Engine.Projection
import Engine.Vector
import Engine.Functor
import Engine.Geometry
import Engine.Normal

type Proj_Displace      a b = Proj a b
type Proj_UV_Displace   a b = Proj_Displace (UV   a) b
type Proj_UVW_Displace  a b = Proj_Displace (UVW  a) b
type Proj_UVWT_Displace a b = Proj_Displace (UVWT a) b

displace
  :: (Scalar a, Vector f)
  => (Proj_Normal   t (f Float)) -- Normal projection.
  -> (Proj_Vertex   t (f Float)) -- Vertex projection.
  -> (Proj_Displace t a)         -- Displacement map.
  -> (Proj_Vertex   t (f Float)) -- Vertex projection.
displace np vp dp uv = vp uv +. scalar (dp uv) *. np uv

displace3_UV
  :: (Fractional t, Scalar a)
  => Proj_UV_Vertex3  t Float
  -> Proj_UV_Displace t a
  -> Proj_UV_Vertex3  t Float
displace3_UV vp = displace (normal3 vp) vp

