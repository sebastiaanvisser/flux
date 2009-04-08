module OpenGL.Aspect.Keyboard (keyboard) where

import Graphics.UI.GLUT
import OpenGL.Setup (Aspect, Callbacks (..), emptyCallbacks, OpenGLEvent)

keyboard
  :: Char -> Char -> Char
  -> Float -> Float -> Float -> Float
  -> Aspect Float

keyboard m p r z s a b o =
  (emptyCallbacks o) {
    cbSetup    = return z
  , cbKeyMouse = dragKeyMouse m p r z s a b o
  }

dragKeyMouse
  :: Char -> Char -> Char
  -> Float -> Float -> Float -> Float
  -> Float
  -> OpenGLEvent -> IO Float

dragKeyMouse m p r z s a b f (k, _, _, _) = do

  case k of
    Char x | x == m -> return (max (f - s) a)
    Char x | x == p -> return (min (f + s) b)
    Char x | x == r -> return z
    _               -> return f


