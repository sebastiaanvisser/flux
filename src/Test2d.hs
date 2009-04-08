module Main where

import Control.Monad
import Engine.Bitmap
import Engine.Vector
import OpenGL.Aspect.DragOrtho
import OpenGL.Aspect.DragPersp
import OpenGL.Aspect.Ortho
import OpenGL.Aspect.Persp
import OpenGL.Aspect.Timer
import OpenGL.Display
import OpenGL.Drawing
import OpenGL.Font
import OpenGL.ProggyOptiS
import OpenGL.Setup
import Scenes.Scenes
import Graphics.Rendering.OpenGL (preservingMatrix, Vector3 (..), translate)
import Graphics.UI.GLUT (TextureObject)

type TO = TextureObject

main :: IO ()
main = setup test3D

-----------------------------------

dragConfig2D :: DragOrthoConfig
dragConfig2D = DragOrthoConfig 1 (V2 6 11) 1

test2D :: (Timer, (TO, TO, String), OrthoConfig) ->
    Callbacks (Timer, (TO, TO, String), OrthoConfig)
test2D ~(t, d, p) = do
  displayPre "sexytime!"
  t' <- timer t
  p' <- dragOrtho dragConfig2D p
  orthographic p'
  d' <- mine2D t' d
  displayPost
  return (t', d', p')

mine2D :: t -> (TO, TO, String) -> Callbacks (TO, TO, String)
mine2D t disp = 
  (emptyCallbacks disp) {
    cbSetup   = mySetup
  , cbDisplay = myDisplay2D t disp
  }

mySetup :: IO (TO, TO, String)
mySetup = do
  Just bmp0 <- readBitmap "textures/opti.rgba"
  Just bmp1 <- readBitmap "textures/sp-star-pink.rgba"
  txt       <- readFile "src/Main.hs"
  font      <- bitmapToGLTexture False bmp0
  star      <- bitmapToGLTexture False bmp1
  return (font, star, txt)

myDisplay2D :: t -> (TO, t1, String) -> IO (TO, t1, String)
myDisplay2D _ (font, star, txt) = do
  preservingMatrix $ do
    translate (Vector3 4 (5.5::Float) 0)
    renderString proggyOptiS txt
  preservingMatrix $ withTexture2D font $ do
    translate (Vector3 4 (5.5::Float) 1)
    renderString proggyOptiS txt
  return (font, star, txt)

