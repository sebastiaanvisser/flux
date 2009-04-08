module OpenGL.Aspect.Persp (PerspConfig (..), perspective) where

import Engine.Vector hiding (Vector3)
import Engine.Geometry (UVW)
import Graphics.UI.GLUT hiding (perspective)
import OpenGL.Setup (Callbacks (..), emptyCallbacks)

data PerspConfig =
  PerspConfig {
--     ocZoom   :: Float
    ocTrans  :: UVW Float
  , ocRotate :: UVW Float
  }

perspective :: PerspConfig -> Callbacks ()
perspective cfg =
  (emptyCallbacks ()) {
      cbReshape = perspReshape cfg
    , cbDisplay = perspDisplay cfg
    }

perspReshape :: t -> Size -> IO ()
perspReshape _ (Size w h) = do

  viewport   $= (Position 0 0, Size w h)
  depthMask  $= Enabled
  matrixMode $= Projection

  loadIdentity

  let w' = (fromIntegral w :: Double)
      h' = (fromIntegral h :: Double)
  frustum (-w'/h') (w'/h') (-1) 1 1 200
  translate (Vector3 0 0 (-10) :: Vector3 Float)

  matrixMode $= Modelview 0

perspDisplay :: PerspConfig -> IO ()
perspDisplay (PerspConfig (V3 tu tv tw) (V3 ru rv rw)) = do
  translate (Vector3 tu tv tw)
  rotate ru $ Vector3 1 0 0
  rotate rv $ Vector3 0 1 0
  rotate rw $ Vector3 0 0 1
  
