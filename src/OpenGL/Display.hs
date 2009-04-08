module OpenGL.Display (displayPre, displayPost) where

import Graphics.UI.GLUT
import OpenGL.Setup (Callbacks (..), emptyCallbacks)

displayPre :: String -> Callbacks ()
displayPre title =
  (emptyCallbacks ()) {
      cbDisplay = dispPre
    , cbSetup   = setupPre title
    }

displayPost :: Callbacks ()
displayPost =
  (emptyCallbacks ()) {
      cbDisplay = swapBuffers
    , cbIdle    = postRedisplay Nothing
    }

dispPre :: IO ()
dispPre = do
  loadIdentity
  clearColor $= Color4 0 0 0 (0 :: GLfloat)
  clear [ColorBuffer, DepthBuffer] -- , StencilBuffer]

setupPre :: String -> IO ()
setupPre title = do

  -- Initialize OpenGL and setup useful display.
  getArgsAndInitialize
  initialDisplayMode $= [
      DoubleBuffered
    , RGBAMode
    , WithAlphaComponent
    , WithDepthBuffer
    , WithAccumBuffer
    , WithStencilBuffer
    ]

  initialDisplayCapabilities $= [
      With  DisplayRGBA
    , Where DisplayDepth IsAtLeast 16
--     , With  DisplaySamples
--     , Where DisplayStencil IsNotLessThan 2
    , Where DisplayAcc IsEqualTo 8
    , With  DisplayDouble
    ]

  createWindow title

  depthFunc                  $= Just Less
  blend                      $= Enabled
  blendFunc                  $= (SrcAlpha, OneMinusSrcAlpha)
  textureFunction            $= Replace
  cullFace                   $= Just Back
--   dither                     $= Enabled
--   shadeModel                 $= Smooth
--   hint PerspectiveCorrection $= Fastest
--   hint PolygonSmooth         $= Fastest


