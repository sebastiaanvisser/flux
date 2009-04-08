module Scenes.Scenes where

import Control.Applicative
import Engine.Bitmap
import Engine.Color
import Engine.Geometry
import Engine.Normal
import Engine.Primitives
import Engine.Modifiers
import Engine.Projection
import Engine.Range
import Engine.Sampling
import Engine.Texture
import Engine.Transform
import Engine.Vector
import OpenGL.Drawing
import OpenGL.SurfaceUV
import Prelude hiding (flip)

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT (($=))

objN :: Int -> UV Float -> Vertex3 Float
objN 1    = planeUV (V2 16 16)
objN 2    = tubeUV 8 (2 :~: 2)
objN 3    = torusUV 3 1
objN _    = undefined

obj :: UV Float -> Vertex3 Float
obj = objN (1::Int)

tangents :: UV Float -> Tangent3 Float
tangents = tangent3 obj

mixer :: Vector2 Float -> Float
mixer = (+0.5) . (*0.5) . sinoid . scale (V2 8 3)

obj' :: UV Float -> Vertex3 Float
obj' = obj -- displace mixer (getN . tangents) obj 

tangents' :: UV Float -> Tangent3 Float
tangents' = tangent3 obj'

sampler :: Vector2 (Sampler Float)
sampler = sharpenF $ linear1F (V2 4  ( 4::Float))

texture :: Vector2 Float -> Color (Tint Float)
texture = gradient_UV ((yellow :~: orange) :~: (blue :~: red))

normal2clr :: Vector3 Float -> Vector4 Float
normal2clr (V3 u v w) = let n x = 0.5 + (-0.5 * x) in V4 (n v) (n u) (n w) 1

createTextures :: IO ()
createTextures =
  do renderBitmapUV  0 True (normal2clr . scale' (pure 2) . getN . tangents') (V2 (256::Float) 256)
     renderBitmapUV  1 True (texture::Texture_UV Float Float)                 (V2 (256::Float) 256)
     readGLTexture2D 2 "textures/monster-normal.rgba"
     readGLTexture2D 3 "textures/monster.rgba"
     return ()

myTest :: Vector3 (Tint Float) -> IO ()
myTest lt = do

--   renderWire $ normal_UV obj' (scale (pure 0.2) . tangS') sampler
--   renderWire $ normal_UV obj' (scale (pure 0.2) . tangT') sampler
--   renderWire $ normal_UV obj' (scale (pure 0.2) . tangN') sampler
--   renderWire $ normal_UV obj' (scale (pure 1) . normalize . trans' lt . obj') sampler

  withNormalMap2D (0, 2) (1, 3) $
    renderTri $ surface_UV_VTTCN
        obj'
          (normal2clr . normalize . trans' lt . obj')
          (getN . tangents')
          sampler

--   renderPoint lt

renderPoint :: Vector3 Float -> IO ()
renderPoint (V3 u v w) =
  GL.preservingMatrix $
    do GL.lighting $= GL.Disabled
       GL.translate (GL.Vector3 u v w)
       GL.cullFace $= Nothing
       renderTri $ surface_UV_VTC (planeUV (V2 2 2)) (solid red) (linear1F (V2 (0.1::Float) 0.1))
       GL.rotate 90 (GL.Vector3 0 1 (0::Float))
       renderTri $ surface_UV_VTC (planeUV (V2 2 2)) (solid green) (linear1F (V2 (0.1::Float) 0.1))
       GL.rotate 90 (GL.Vector3 1 0 (0::Float))
       renderTri $ surface_UV_VTC (planeUV (V2 2 2)) (solid blue) (linear1F (V2 (0.1::Float) 0.1))
       GL.cullFace $= Just GL.Back
       GL.lighting $= GL.Enabled

