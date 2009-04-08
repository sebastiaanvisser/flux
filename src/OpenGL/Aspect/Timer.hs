module OpenGL.Aspect.Timer (Timer (..), timer) where

import Engine.Timer
import OpenGL.Setup

data Timer =
  Timer {
    elapsed :: Double
  , start   :: Double
  , frames  :: Integer
  , fps     :: Integer
  }

timer :: Aspect Timer
timer t =
  (emptyCallbacks t) {
    cbSetup   = setupTimer 
  , cbDisplay = displayTimer t
  }

setupTimer :: IO Timer
setupTimer = do
  t <- currentTick
  return (Timer t t 0 0)

displayTimer :: Timer -> IO Timer
displayTimer (Timer t s f _) =  do
  t' <- currentTick
  return (Timer t' s (f + 1) (round (1 / (t'-t))))

