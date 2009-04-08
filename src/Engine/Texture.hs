module Engine.Texture where

import Control.Monad
import Control.Applicative
import Foreign
import Engine.Bitmap
import Engine.Functor
import Engine.Color
import Engine.Geometry
import Engine.Interpolation
import Engine.Sampling
import Engine.Projection
import Engine.Vector
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL

--------[ texture mapping ]----------------------------------------------------

type Texture      a b = Proj a (Color b)
type Texture_UV   a b = Texture (UV   a) b
type Texture_UVW  a b = Texture (UVW  a) b
type Texture_UVWT a b = Texture (UVWT a) b

--------[ render UV texture to bitmap ]----------------------------------------

{-renderGlyph
  :: UV Int -> Bool
  -> [Color Float] -> IO GL.TextureObject
renderGlyph (V2 w h) smooth  pxls = do
  ptr <- words32toPtr $ map color4toWord32 pxls
  bitmapToGLTexture smooth (Bitmap w h ptr)-}

renderBitmapUV
  :: (RealFrac t, Enum t, Ipol a, Scalar t, Num a)
  => GL.GLuint -> Bool -> Texture_UV a Float -> UV t -> IO ()
renderBitmapUV i smooth tx (V2 w h) = do
  GL.textureBinding GL.Texture2D $= Just (GL.TextureObject i)
  let pxls = renderPixelsUV (V2 w h) tx
  ptr <- words32toPtr pxls
  bitmapToGLTexture smooth (Bitmap (floor w) (floor h) ptr)

renderPixelsUV
  :: (Fractional t, Enum t, Ipol a, Scalar t, Num a) =>
  UV t -> (Texture_UV a Float) -> [Word32]
renderPixelsUV (V2 w h) tx =
  do let (V2 us vs) = linear1 (V2 (w-1) (h-1))
     (\u v -> color4toWord32 $ tx $ V2 u v) <$> us <*> vs

color4toWord32 :: Color Float -> Word32
color4toWord32 (V4 r g b a) =
  let f x = round (x*255) in
  word8x4toNum (f a) (f b) (f g) (f r)

words32toPtr :: [Word32] -> IO (Ptr Word32)
words32toPtr xs = do
  let bytes = length xs * sizeOf (0::Word32)
  buf <- mallocBytes bytes
  zipWithM_ (\i e -> pokeElemOff buf i e) [0..] xs
  return buf

