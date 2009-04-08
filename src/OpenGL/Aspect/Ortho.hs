module OpenGL.Aspect.Ortho (OrthoConfig (..), orthographic) where

import Graphics.UI.GLUT
import OpenGL.Setup (Callbacks (..), emptyCallbacks)
import Engine.Geometry (UV)
import Engine.Vector (Vector2 (..))

data OrthoConfig =
  OrthoConfig {
    ocZoom   :: Float
  , ocTrans  :: UV Float
  , ocRotate :: Float
  }

orthographic :: OrthoConfig -> Callbacks ()
orthographic cfg =
  (emptyCallbacks ()) {
      cbReshape = orthoReshape cfg
    , cbDisplay = orthoDisplay cfg
    }

orthoReshape :: OrthoConfig -> Size -> IO ()
orthoReshape _ (Size w h) = do

  viewport   $= (Position 0 0, Size w h)
  depthMask  $= Enabled
  matrixMode $= Projection

  loadIdentity

  let w' = (fromIntegral w) :: Double
      h' = (fromIntegral h) :: Double
      x' = 0 :: Double
      y' = 0 :: Double
  ortho x' (x' + w') (y' - h') y' (-1000) (1000)
--   ortho (-w'/2) (w'/2) (-h'/2) (h'/2) (-1000) (1000)

  matrixMode $= Modelview 0

orthoDisplay :: OrthoConfig -> IO ()
orthoDisplay (OrthoConfig s (V2 x y) r) = do
  loadIdentity
  rotate r (Vector3 0 0 1)
  scale s (-s) s
  translate (Vector3 x y 0)

