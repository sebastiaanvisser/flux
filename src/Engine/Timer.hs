module Engine.Timer where

import Data.Time.Clock (getCurrentTime, utctDayTime)
import Engine.Math (pi2)

currentTick :: IO Double
currentTick = getCurrentTime >>= return . fromRational . toRational . utctDayTime

milliseconds :: Fractional a => a -> a
milliseconds = (*0.001)

seconds :: Num a => a -> a
seconds = (*1)

minutes :: Num a => a -> a
minutes = (*60)

hours :: Num a => a -> a
hours = (*3600)

every :: Double -> (Double -> IO a) -> IO a
every p f = do
  t <- currentTick
  f (0.5 + 0.5 * sin (t/p*pi2))

within :: Double -> Double -> (Double -> IO a) -> IO a
within a d f = do
  t <- currentTick
  f (t * a / d)
