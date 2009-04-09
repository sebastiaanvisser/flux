module Engine.Surface where

import Engine.Projection
import Engine.Normal
import Engine.Texture
import Engine.Sampling

data Surface3 a b =
  Surface3 {
    sSurface  :: Maybe (Proj_UV_Vertex3 a b)
  , sNormals  :: Maybe (Proj_UV_Normal3 a b)
  , sColors   :: Maybe (Texture_UV      a b)
  , sCoords   :: [Proj_UV_UV a b]
  , sSampling :: Sampler_UV a
  }

