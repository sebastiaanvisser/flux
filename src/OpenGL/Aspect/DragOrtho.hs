module OpenGL.Aspect.DragOrtho (DragOrthoConfig (..), dragOrtho) where

import Control.Applicative ((<*>), (<$>))
import Engine.Geometry (setu, getu, setv, getv, UV)
import Engine.Vector (Vector2 (..))
import Graphics.UI.GLUT
import OpenGL.Aspect.Ortho (OrthoConfig (..))
import OpenGL.Setup (Aspect, Callbacks (..), emptyCallbacks, OpenGLEvent)

data DragOrthoConfig =
  DragOrthoConfig {
    dcZoom   :: Float
  , dcTrans  :: UV Float
  , dcRotate :: Float
  }

dragOrtho :: DragOrthoConfig -> Aspect OrthoConfig
dragOrtho d o =
  (emptyCallbacks o) {
    cbSetup    = return (OrthoConfig 1 (V2 0 0) 0)
  , cbKeyMouse = dragKeyMouse d o
  }

dragKeyMouse :: DragOrthoConfig -> OrthoConfig -> OpenGLEvent -> IO OrthoConfig
dragKeyMouse (DragOrthoConfig dZ dT dR) b@(OrthoConfig pZ pT pR) (k, s, m, _) = do

  -- Comput
  let mT = (+) <$> pT <*> dT
      lT = (-) <$> pT <*> dT
      mZ = (+)     pZ     dZ
      lZ = (-)     pZ     dZ
      mR = (+)     pR     dR
      lR = (-)     pR     dR

  case (s, shift m, k) of
    (Down, _,    Char '0'           ) -> return (OrthoConfig 1  (V2 0 0)            0)
    (Down, Up,   SpecialKey KeyLeft ) -> return (OrthoConfig pZ (setu (getu mT) pT) pR)
    (Down, Up,   SpecialKey KeyRight) -> return (OrthoConfig pZ (setu (getu lT) pT) pR)
    (Down, Up,   SpecialKey KeyUp   ) -> return (OrthoConfig pZ (setv (getv mT) pT) pR)
    (Down, Up,   SpecialKey KeyDown ) -> return (OrthoConfig pZ (setv (getv lT) pT) pR)
    (Down, Down, SpecialKey KeyUp   ) -> return (OrthoConfig mZ pT                  pR)
    (Down, Down, SpecialKey KeyDown ) -> return (OrthoConfig lZ pT                  pR)
    (Down, Down, SpecialKey KeyLeft ) -> return (OrthoConfig pZ pT                  mR)
    (Down, Down, SpecialKey KeyRight) -> return (OrthoConfig pZ pT                  lR)
    _                                 -> return b

