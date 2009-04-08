module Engine.Projection where

import Engine.Interpolation
import Engine.Range
import Engine.Vector
import Engine.Functor
import Engine.Geometry

type Proj      a b = a -> b
type Proj_UV   a b = Proj (UV   a) b
type Proj_UVW  a b = Proj (UVW  a) b
type Proj_UVWT a b = Proj (UVWT a) b

type Proj_Vertex       a b = Proj a b

type Proj_Vertex3      a b = Proj a (Vertex3 b)
type Proj_UV_Vertex3   a b = Proj_Vertex3 (UV   a) b
type Proj_UVW_Vertex3  a b = Proj_Vertex3 (UVW  a) b
type Proj_UVWT_Vertex3 a b = Proj_Vertex3 (UVWT a) b

--------[ common projections ]-------------------------------------------------

solid :: b -> Proj a b
solid = const

gradient_U :: (Vector f, Ipol a, Scalar b) => Range a -> Proj (f b) a
gradient_U g v = g ..< getU v

gradient_V :: (Vector f, Ipol a, Scalar b) => Range a -> Proj (f b) a
gradient_V g v = g ..< getV v

gradient_W :: (Vector f, Ipol a, Scalar b) => Range a -> Proj (f b) a
gradient_W g v = g ..< getW v

gradient_UV :: (Ipol a, Scalar t) => Range (Range a) -> Vector2 t -> a
gradient_UV (g :~: h) (V2 u v) = (g ..< u) :~: (h ..< u) ..< v

-- todo: compact
-- radialGradientUV :: UV Float -> UV Float -> Range (Color Float) -> TextureUV
-- radialGradientUV (V2 tU tY) (V2 sU sV) g (V2 u _) =
--   g ..< sqrt (((tU-u)/sU)^(2::Int) + ((tY-u)/sV)^(2::Int))

-- defaultRadialGradientUV :: Range (Color Float) -> TextureUV
-- defaultRadialGradientUV = radialGradientUV (V2 0.5 0.5) (V2 0.5 0.5)

