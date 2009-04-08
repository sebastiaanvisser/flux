module Scenes.Examples.Test where

import Data.Int
import Data.List
import Control.Applicative
import Engine.Storable
-- import Engine.Bitmap
import Engine.Color
import Engine.Geometry
import Engine.Modifiers
import Engine.Normal
import Engine.Primitives
import Engine.Projection
import Engine.Range
import Engine.Sampling
import Engine.Texture
import Engine.Transform
import Engine.Vector
-- import OpenGL.Drawing
import OpenGL.Setup
import OpenGL.SurfaceUV
-- import Prelude hiding (flip)
import Graphics.UI.GLUT (($=))
import qualified Graphics.Rendering.OpenGL as GL
import Foreign

test ~(ptr, _N, o) = 
  (emptyCallbacks (ptr, _N, o)) {
    cbDisplay = renderSurface
  , cbSetup   = setupSurface
  }
  where

    _S  = 4 + 2 + 3 + 3 :: Int
    _F  = sizeOf (undefined :: Float)

--     vtx = [V3 (-10) 0 (-10), V3 (-10) 0 10, V3 10 0 10, V3 10 0 (-10)] :: [Vector3 Float]
--     clr = [V4 1 0 0 1, V4 1 1 0 1, V4 0 1 1 1, V4 1 0 1 1] :: [Vector4 Float]
--     tex = [V2 0 0, V2 1 0, V2 1 1, V2 0 1] :: [Vector2 Float]
--     nrm = replicate 4 (V3 0 1 0) :: [Vector3 Float]
--     atr = zipWith4 (\a b c d -> (a, b, c, d)) vtx clr tex nrm

    setupSurface =
      do [o] <- (GL.genObjectNames 1) :: IO [GL.BufferObject]
         GL.bindBuffer GL.ArrayBuffer $= Just o

         (fptr, _N) <- sf_UV_VTCN_alloc
           obj
           texture
           (getN . tangents)
           sampler

         print ("elements:   " , _N)
         print ("total size: " , _F * _S * _N)

         withForeignPtr fptr $ \p ->
           do GL.bufferData GL.ArrayBuffer $= (fromIntegral (_F * _S * _N), nullPtr, GL.StaticDraw)
              GL.bufferSubData GL.ArrayBuffer GL.WriteToBuffer 0 (fromIntegral (_F * _S * _N)) p

         GL.bindBuffer GL.ArrayBuffer $= Just o

         return (fptr, _N, o)

    renderSurface =
      do 
--          withNormalMap2D (0, 2) (1, 3)
--            $ renderTri
--            $ surface_UV_VTCN
--              obj
--              texture
--              (normal3 obj)
--              sampler

         GL.clientState GL.ColorArray         $= GL.Enabled
         GL.clientState GL.TextureCoordArray  $= GL.Enabled
         GL.clientState GL.NormalArray        $= GL.Enabled
         GL.clientState GL.VertexArray        $= GL.Enabled

         GL.arrayPointer GL.ColorArray        $= GL.VertexArrayDescriptor 4 GL.Float (fromIntegral (_S * _F)) (nullPtr `plusPtr` (0 * _F))
         GL.arrayPointer GL.TextureCoordArray $= GL.VertexArrayDescriptor 2 GL.Float (fromIntegral (_S * _F)) (nullPtr `plusPtr` (4 * _F))
         GL.arrayPointer GL.NormalArray       $= GL.VertexArrayDescriptor 3 GL.Float (fromIntegral (_S * _F)) (nullPtr `plusPtr` (6 * _F))
         GL.arrayPointer GL.VertexArray       $= GL.VertexArrayDescriptor 3 GL.Float (fromIntegral (_S * _F)) (nullPtr `plusPtr` (9 * _F))

         GL.bindBuffer GL.ArrayBuffer $= Just o


         GL.colorMaterial              $= Just (GL.Front, GL.Diffuse) 
         GL.materialDiffuse   GL.Front $= (GL.Color4 0.0 0.0 0.0 0.0)
         GL.materialAmbient   GL.Front $= (GL.Color4 0.0 0.0 0.0 0.0)
         GL.materialSpecular  GL.Front $= (GL.Color4 1.0 1.0 1.0 1.0)
         GL.materialEmission  GL.Front $= (GL.Color4 0.0 0.0 0.0 0.0)
         GL.materialShininess GL.Front $= 80

         GL.drawArrays GL.TriangleStrip 0 (fromIntegral _N)

         return (ptr, _N, o)

    sampler  = linear1 (V2 512 (192::Float)) :: Sampler_UV Float
    texture  = mix mixer 
                 (gradient_UV ((red :~: yellow) :~: (black :~: blue)) . tile (V2 8 4))
                 (solid magenta)
                 :: Texture_UV Float Float
    mixer    = fmap (**4) . bias . sinoid . scale (V2 23 9)

    obj0     = {-spiralUV (1 :~: 3) (0.2 :~: 0.8) 4 3-} {-planeUV (V2 10 10)-} torusUV 10 2 :: Proj_UV_Vertex3 Float Float
    obj      = displace3_UV obj0 (scale (pure 1.0) . (\i -> (+) <$> (sinoid . scale (V2 7 0)) i <*> mixer i))
    tangents = tangent3 obj


