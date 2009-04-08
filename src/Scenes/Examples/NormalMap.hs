module Scenes.Examples.NormalMap where

import Control.Applicative
import Engine.Bitmap
import Engine.Color
import Engine.Normal
import Engine.Primitives
import Engine.Projection
import Engine.Range
import Engine.Sampling
import Engine.Texture
import Engine.Transform
import Engine.Vector
import OpenGL.Drawing
import OpenGL.Setup
import OpenGL.SurfaceUV
import Prelude hiding (flip)

normalMapExample :: Vector3 Float -> Callbacks ()
normalMapExample light = 
  (emptyCallbacks ()) {
    cbSetup   = createTextures
  , cbDisplay = renderSurface
  }
  where

    createTextures =
      do renderBitmapUV  0 True (normal2clr . scale' (pure 2) . getN . tangents) (V2 (256::Float) 256)
         renderBitmapUV  1 True (texture::Texture_UV Float Float)                 (V2 (256::Float) 256)
         readGLTexture2D 2 "textures/monster-normal.rgba"
         readGLTexture2D 3 "textures/monster.rgba"
         return ()

    renderSurface =
      withNormalMap2D (0, 2) (1, 3)
        $ renderTri
        $ surface_UV_VTTCN
          obj
          (normal2clr . normalize . trans' light . obj)
          (getN . tangents)
          sampler

    obj        = planeUV (V2 10 10)
    tangents   = tangent3 obj
    sampler    = sharpen $ linear1 (V2 4  ( 4::Float))
    texture    = gradient_UV ((yellow :~: orange) :~: (blue :~: red))

    -- Wrong:
    normal2clr = (\(V3 u v w) -> V4 v u w 1) . bias . flip


