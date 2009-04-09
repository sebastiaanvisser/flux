module Engine.Volume where

import Engine.Projection
import Engine.Normal
import Engine.Texture
import Engine.Sampling

data Volume3 a b =
  Volume3 {
    vSurface  :: Maybe (Proj_UVW_Vertex3 a b)
  , vNormals  :: Maybe (Proj_UVW_Normal3 a b)
  , vColors   :: Maybe (Texture_UVW      a b)
  , vCoords   :: [Proj_UVW_UVW a b]
  , vSampling :: Sampler_UVW a
  }


