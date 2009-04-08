module OpenGL.Aspect.DragPersp (DragPerspConfig (..), dragPersp) where

import Control.Applicative ((<$>), (<*>), pure)
import Engine.Vector (Vector3 (..))
import Engine.Geometry
import OpenGL.Setup (Callbacks (..), emptyCallbacks, Aspect, OpenGLEvent)
import OpenGL.Aspect.Persp (PerspConfig (..))
import Graphics.UI.GLUT

 -- min, max, step
data DragPerspConfig =
  DragPerspConfig {
    dcTrans   :: UVW Float
  , dcRotate  :: UVW Float
  }

dragPersp :: DragPerspConfig -> Aspect PerspConfig
dragPersp d p =
  (emptyCallbacks p) {
    cbSetup    = return (PerspConfig (V3 0 0 0) (V3 0 0 0))
  , cbKeyMouse = dragKeyMouse d p
  }

dragKeyMouse :: DragPerspConfig -> PerspConfig -> OpenGLEvent -> IO PerspConfig
dragKeyMouse (DragPerspConfig dT dR) b@(PerspConfig pT pR) (k, s, m, _) = do

  let mT = (+) <$> pT <*> dT
      lT = (-) <$> pT <*> dT
      mR = (+) <$> pR <*> dR
      lR = (-) <$> pR <*> dR

  case (s, shift m, alt m, k) of

    (Down, Up,   Down, SpecialKey KeyUp   ) -> return (PerspConfig (setW (getW mT) pT) pR)
    (Down, Up,   Down, SpecialKey KeyDown ) -> return (PerspConfig (setW (getW lT) pT) pR)

    (Down, Down, Up,   SpecialKey KeyLeft ) -> return (PerspConfig pT (setV (getV mR) pR))
    (Down, Down, Up,   SpecialKey KeyRight) -> return (PerspConfig pT (setV (getV lR) pR))
    (Down, Down, Up,   SpecialKey KeyUp   ) -> return (PerspConfig pT (setU (getU lR) pR))
    (Down, Down, Up,   SpecialKey KeyDown ) -> return (PerspConfig pT (setU (getU mR) pR))
    (Down, Up,   Down, SpecialKey KeyLeft ) -> return (PerspConfig pT (setW (getW mR) pR))
    (Down, Up,   Down, SpecialKey KeyRight) -> return (PerspConfig pT (setW (getW lR) pR))

    (Down, Up,   Up,   Char '0'           ) -> return (PerspConfig (pure 0) (pure 0))

    _  -> return b

