module Grid.Grid where

import Engine.Geometry
import Engine.Color

data Region = 
    Single Point2i
  | Row    Point2i Int
  | Col    Point2i Int
  | Block  Rectangle2i

data Plot = 
    Line Color Region
  | Fill Color Region
  | Chr  Color 

