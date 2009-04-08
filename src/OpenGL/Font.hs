module OpenGL.Font where

import Engine.Primitives (planeUV)
import Engine.Math (just)
import Engine.Interpolation (gradient1UV)
import Engine.Font (lookupGlyph, Font (..))
import OpenGL.Drawing (drawTriangleStrip)
import Engine.Vector (Vector2 (..))
import Engine.Range (Range (..))
import OpenGL.SurfaceUV (surfaceUV_VTC, render)
import Engine.Color (blue, black, red, orange)
import Engine.Texture (uvGradientUV)
import Engine.Transform (scale', trans)
import Graphics.UI.GLUT hiding (Font)

renderChar :: Font -> Char -> IO ()
renderChar f c = do
  let w  = fromIntegral (ftWidth       f)
      h  = fromIntegral (ftHeight      f)
      gw = fromIntegral (ftGlyphWidth  f)
      gh = fromIntegral (ftGlyphHeight f)
  case lookupGlyph f c of
    Just (V2 x y, _) -> do
      drawTriangleStrip $ render $ surfaceUV_VTC
          (planeUV $ V2 gw gh)
          gradient1UV
          (scale' (V2 (just w) (just h)) . trans (V2 (fromIntegral x) (fromIntegral y)))
          (if
            (c `elem` ['a'..'z']) ||
            (c `elem` ['A'..'Z']) ||
            (c `elem` ['0'..'9']) 
            then (uvGradientUV ((black :~: black) :~: (orange :~: red)))
            else (uvGradientUV ((black :~: black) :~: (blue :~: black)))
              )
      translate (Vector3 gw 0 0)
    Nothing -> return ()

renderLine :: Font -> String -> IO ()
renderLine = mapM_ . renderChar

renderString :: Font -> String -> IO ()
renderString f s =
  let gh = fromIntegral (ftGlyphHeight f) in
  flip mapM_ (lines s) $ \l -> do
    preservingMatrix (renderLine f l)
    translate (Vector3 0 (gh :: Float) 0)

