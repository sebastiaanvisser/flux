module Scenes.Examples.Test where

import Control.Applicative
import Engine.Color
import Engine.Modifiers
import Engine.Normal
import Engine.Primitives
import Engine.Projection
import Engine.Range
import Engine.Sampling
import Engine.Surface
import Engine.Texture
import Engine.Transform
import Engine.Vector
import Foreign
import Graphics.UI.GLUT (($=))
import OpenGL.Setup
import OpenGL.SurfaceUV
import System.IO
import qualified Graphics.Rendering.OpenGL as GL

--          fh <- openBinaryFile "data/torus0.obj" ReadWriteMode
--          withForeignPtr fptr $ \p -> hGetBuf fh p (n * s)
--          withForeignPtr fptr $ \p -> hPutBuf fh p (n * s)
--          hClose fh

test o = 
  (emptyCallbacks o) {
    cbDisplay = renderSurface
  , cbSetup   = setupSurface
  }
  where

    setupSurface =
      do [o'] <- (GL.genObjectNames 1) :: IO [GL.BufferObject]
         GL.bindBuffer GL.ArrayBuffer $= Just o'
         (n, s, _) <- renderToVBO mySurface o'
         return o'

    renderSurface =
      do GL.colorMaterial              $= Just (GL.Front, GL.Diffuse) 
         GL.materialDiffuse   GL.Front $= (GL.Color4 0.0 0.0 0.0 0.0)
         GL.materialAmbient   GL.Front $= (GL.Color4 0.0 0.0 0.0 0.0)
         GL.materialSpecular  GL.Front $= (GL.Color4 1.0 1.0 1.0 1.0)
         GL.materialEmission  GL.Front $= (GL.Color4 0.0 0.0 0.0 0.0)
         GL.materialShininess GL.Front $= 80
         display o mySurface
         return o

mySurface =
  Surface3 {
    sSurface  = Just obj
  , sNormals  = Just (getN . tangents)
  , sCoords   = [id]
  , sColors   = Just texture
  , sSampling = sampler
  }

sampler  = linear1 (V2 128 (64::Float)) :: Sampler_UV Float
texture  = mix mixer 
             (gradient_UV ((red :~: yellow) :~: (black :~: blue)) . tile (V2 8 4))
             (solid magenta)
             :: Texture_UV Float Float
mixer    = fmap (**3) . bias . sinoid . scale (V2 33 9)

obj0     = torusUV 10 2 :: Proj_UV_Vertex3 Float Float
obj      = displace3_UV obj0 (scale (pure 1.0) . (\i -> (+) <$> (sinoid . scale (V2 7 0)) i <*> mixer i))
tangents = tangent3 obj


