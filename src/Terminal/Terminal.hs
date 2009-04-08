module Terminal.Terminal where

import Data.Bits
import Data.IntMap
import qualified Data.Ix as Ix

import Engine.Range
import Engine.Types
import Engine.Vector
import Engine.Geometry

--------------------------------------------------------------------------------

{-
A fast lookup grid with maximum boundries defined by `Prelude.minBound :: Int'
and `Prelude.maxBound :: Int', based on `IntMap's.
-}

{-data Grid a =
  Grid {
    rect   :: Rectangle2d Int
  , imap   :: IntMap a
  }
  deriving Show

modMap :: (IntMap a -> IntMap b) -> Grid a -> Grid b
modMap f g = g { imap = f (imap g) }

pack :: Int -> Int -> Int
pack x y = (y `shiftL` 0xF) + x

unpack :: Int -> Point2d Int
unpack i = V2 (i .&. 0xFFFF) (i `shiftR` 0xF)

mkGrid :: Rectangle2d Int -> a -> Grid a
mkGrid r@(V2 x0 y0 :~: V2 x1 y1) a =
    Grid r
  $ foldl (\x y -> insert y a x) empty
  $ [ pack x y
    | x <- [x0..x1]
    , y <- [y0..y1]
    ]

sel :: Point2d Int -> Grid a -> a
sel (V2 x y) = (! pack x y) . imap

row :: Int -> Grid a -> [a]
row r = elems . filterWithKey (\ix _ -> gety (unpack ix) == r) . imap

col :: Int -> Grid a -> [a]
col c = elems . filterWithKey (\ix _ -> getx (unpack ix) == c) . imap

sub :: Rectangle2d Int -> Grid a -> Grid a
sub r = modMap $ filterWithKey (\ix _ -> inRect (unpack ix) r ) 
-}






-- testGrid = mkGrid 4 8 20 5 True


