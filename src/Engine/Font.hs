module Engine.Font where

import Engine.Types
import Engine.Range
import Engine.Vector
import Engine.Geometry
import Control.Applicative
import Data.Char (ord)

-------------------------------------------------------------------------------

choice :: (Alternative f) => [f a] -> f a
choice = foldl (<|>) empty

-------------------------------------------------------------------------------

data IndexBlock = IndexBlock (Offset Int) Rect2i FilePath
type IndexMap = [IndexBlock]

indexBlock :: IndexBlock -> Index -> Maybe (Point2i, FilePath)
indexBlock (IndexBlock off rect@(V2 l t :~: V2 w _) path) ix =
  let i = ix - off in
  if inRange i (0 :~: (surface2 rect - 1))
  then Just ((V2 (l + (i `mod` w)) (t + (i `div` w))), path)
  else Nothing

lookupGlyph :: Font -> Char -> Maybe (Point2i, FilePath)
lookupGlyph ft ix = choice $ map (flip indexBlock $ ord ix) $ ftMap ft

data Font =
  Font {
    ftName        :: String
  , ftWidth       :: Width  Int
  , ftHeight      :: Height Int
  , ftGlyphWidth  :: Width  Int
  , ftGlyphHeight :: Height Int
  , ftMap         :: IndexMap
  }

-------------------------------------------------------------------------------

-- Example map and block.
-- Storing ? and reverse ?
-- Storing A upto H.
-- Storing I upto P.

--             <--  10  -->
--    +=====+--+--+--+--+--+--+--+--+
--    | ?| ?|  |  |  | 5|  |  |  | 9|
--    +=====#=====#--+--+--+--+--+--+
--  ^ |10|  | I| J|  |  |  |  |  |  |
--  | +--+--| -+- |--#===========#--+
--    |20|  | K| L|  | A| B| C| D|  |
--  6 +--+--| -+- |--| -+--+--+- |--+
--    |30|  | M| N|  | E| F| G| H|  |
--  | +--+--| -+- |--#===========#--+
--  " |40|  | O| P|  |  |  |  |  |  |
--    +--+--#=====#--+--+--+--+--+--+
--    |50|  |  |  |  |  |  |  |  |  |
--    +--+--+--+--+--+--+--+--+--+--+

testFont :: Font
testFont =
  Font {
    ftName        = "Test-Font"
  , ftWidth       = 10
  , ftHeight      = 6
  , ftGlyphWidth  = 6
  , ftGlyphHeight = 11
  , ftMap         = [
      IndexBlock 63 (V2 0 0 :~: V2 2 1) "www/images/fonts/test-font.png"
    , IndexBlock 65 (V2 5 2 :~: V2 4 2) "www/images/fonts/test-font.png"
    , IndexBlock 73 (V2 2 1 :~: V2 2 4) "www/images/fonts/test-font.png"
    ]
  }

