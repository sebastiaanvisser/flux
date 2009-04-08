module Engine.Bitmap where

import Control.Monad
import Data.Char (ord)
import Engine.Vector
import Engine.Math
import Engine.Projection
import Foreign
import Graphics.Rendering.OpenGL hiding (Bitmap)
import System.IO
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString as B

type Width  = Int
type Height = Int

data Bitmap = Bitmap Int Int (Ptr Word32)
  deriving Show

-------------------------------------------------------------------------------

readBitmap :: FilePath -> IO (Maybe Bitmap)
readBitmap filename = do
  f <- openBinaryFile filename ReadMode 
  w <- read32 f
  h <- read32 f
  let size = fromIntegral (w * h * 4)
  buf <- mallocBytes size
  rd <- hGetBuf f buf size
  hClose f
  return $ if (rd /= size)
    then Nothing
    else Just (Bitmap w h buf)

bitmapToGLTexture :: Bool -> Bitmap -> IO ()
bitmapToGLTexture smooth (Bitmap w h pixels) = do
  if smooth
    then textureFilter Texture2D $= ((Linear', Nothing), Linear')
    else textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
  texImage2D
    Nothing NoProxy 0 RGBA'
    (TextureSize2D (fromIntegral w) (fromIntegral h)) 0
    (PixelData RGBA UnsignedByte pixels)

readGLTexture2D :: GLuint -> FilePath -> IO Bool
readGLTexture2D i fn = do
  textureBinding Texture2D $= Just (TextureObject i)
  mb <- readBitmap fn
  case mb of
    Nothing -> return False
    Just b -> 
      do bitmapToGLTexture True b
         return True

-- todo: refactor
bitmapProj
  :: (Almost b, RealFrac b, RealFrac c)
  => Bitmap -> IO (Proj_UV b (Vector4 c))
bitmapProj bmp@(Bitmap w h _) = do
  let w' = fromIntegral w
      h' = fromIntegral h
  bs <- bufferAsByteString bmp
  return $ \(V2 u v) -> (fmap (\a -> fromIntegral a / 255)) $ bsIndex32 V4 bs ((floor (frac u * w') + w * floor (frac v * h')) * 4)

-------------------------------------------------------------------------------

bufferAsByteString :: Bitmap -> IO B.ByteString
bufferAsByteString (Bitmap w h p) = do
  fp <- newForeignPtr_ p
  return (BI.fromForeignPtr (castForeignPtr fp) 0 (w * h * 4))

read8, read16, read32 :: Handle -> IO Int
read8  h = ord `liftM` hGetChar h
read16 h = liftM2 (+) (liftM (*0x100)   (read8  h)) (read8  h)
read32 h = liftM2 (+) (liftM (*0x10000) (read16 h)) (read16 h)

bsIndex32
  :: (Word8 -> Word8 -> Word8 -> Word8 -> t)
  -> B.ByteString -> Int -> t
bsIndex32 f b i = f
  (B.index b i    )
  (B.index b (i+1)) 
  (B.index b (i+2)) 
  (B.index b (i+3)) 

word8x4toNum :: Num a => Word8 -> Word8 -> Word8 -> Word8 -> a
word8x4toNum a b c d =
  let v = 0x100 in
      fromInteger
    ( v*v*v*toInteger a
    + v*v*  toInteger b
    + v*    toInteger c
    +       toInteger d)

