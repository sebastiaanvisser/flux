module Scenes.Examples.RotatingLight where

import Control.Monad
import Engine.Vector
import OpenGL.Aspect.Keyboard
import OpenGL.Setup
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT (($=))

rotatingLight :: Float -> Float -> Vector3 Float -> Callbacks (Vector3 Float, Vector3 Float)
rotatingLight radius height ~(V3 u v w) = 
  do u' <- keyboard '4' '6' '5' 0      0.01 (-1000) 1000 u
     v' <- keyboard '2' '8' '5' radius 0.1  (0)     1000 v
     w' <- keyboard '1' '7' '5' height 0.1  (-1000) 1000 w
     let rot = V3 (v' * cos (pi * 2 * u')) (v' * sin (pi * 2 * u')) w'
     lighting rot
     return (V3 u' v' w', rot)

  where

    lighting lt = 
      (emptyCallbacks ()) {
        cbSetup   = lightSetup
      , cbDisplay = lightDisplay lt
      }

    lightSetup = 
      do GL.lighting $= GL.Enabled
         GL.diffuse (GL.Light 0) $= GL.Color4 1.0 1.0 1.0 0.0
         GL.ambient (GL.Light 0) $= GL.Color4 0.0 0.0 0.0 0.0
         GL.light   (GL.Light 0) $= GL.Enabled

    lightDisplay (V3 x y z) =
      GL.position (GL.Light 0) $= GL.Vertex4 x y z 0.0

