module Engine.Color where

import Data.Word
import Data.Bits
import Engine.Vector

--------[ colors ]-------------------------------------------------------------

type Tint    a = a
type Opacity a = Tint a
type Color   a = Vector4 (Tint a)

color :: Tint a -> Tint a -> Tint a -> Opacity a -> Color a
color = V4

black, gray, white, red, green, blue, orange, magenta, yellow, nocolor :: Fractional a => Color a
black   = color 0.0 0.0 0.0 1.0
gray    = color 0.5 0.5 0.5 1.0
white   = color 1.0 1.0 1.0 1.0
red     = color 1.0 0.0 0.0 1.0
green   = color 0.0 1.0 0.0 1.0
blue    = color 0.0 0.0 1.0 1.0
orange  = color 1.0 0.5 0.0 1.0
magenta = color 1.0 0.0 1.0 1.0
yellow  = color 1.0 1.0 0.0 1.0
nocolor = color 0.0 0.0 0.0 0.0

-- setRed, setGreen, setBlue, setAlpha :: Tint a -> Color a -> Color a
-- setRed   r (V4 _ g b a) = V4 r g b a
-- setGreen g (V4 r _ b a) = V4 r g b a
-- setBlue  b (V4 r g _ a) = V4 r g b a
-- setAlpha a (V4 r g b _) = V4 r g b a

int2clr :: Word32 -> Color Word8
int2clr w = V4
  (fromIntegral ((w .&. 0x00FF0000) `shiftR` 0x10))
  (fromIntegral ((w .&. 0x0000FF00) `shiftR` 0x08))
  (fromIntegral ((w .&. 0x000000FF)              ))
  (fromIntegral ((w .&. 0xFF000000) `shiftR` 0x18))

