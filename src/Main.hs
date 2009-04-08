module Main where

import Control.Monad
import Engine.Vector
-- import Graphics.UI.GLUT (($=))
import OpenGL.Aspect.DragPersp
-- import OpenGL.Aspect.Keyboard
import OpenGL.Aspect.Persp
import OpenGL.Aspect.Timer
import OpenGL.Display
import OpenGL.Setup
import Scenes.Examples.Test
import Scenes.Examples.RotatingLight
-- import qualified Graphics.Rendering.OpenGL as GL

main :: IO ()
main = setup test3D
  where
    test3D ~(t, d, p, q, ptr, light) =
      do displayPre "sexytime!"
         t' <- timer t
         p' <- dragPersp (DragPerspConfig (V3 1 1 1) (V3 2 2 2)) p
         (light', lt) <- rotatingLight 10 10 light
         perspective p'
         ptr' <- test ptr
         displayPost
         return (t', d, p', q, ptr', light')

