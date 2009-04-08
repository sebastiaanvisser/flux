module OpenGL.Setup where

import Data.IORef (IORef, newIORef)
import Graphics.UI.GLUT

-------------------------------------------------------------------------------

type OpenGLEvent = (Key, KeyState, Modifiers, Position)
data Callbacks st =
  Callbacks {
    cbSetup    ::                IO st
  , cbDisplay  ::                IO st
  , cbIdle     ::                IO st
  , cbReshape  :: Size        -> IO st
  , cbKeyMouse :: OpenGLEvent -> IO st
  }

type Aspect st       = st -> Callbacks st

emptyCallbacks :: st -> Callbacks st
emptyCallbacks st =
  Callbacks {
    cbSetup    = return st
  , cbDisplay  = return st
  , cbIdle     = return st
  , cbReshape  = const (return st)
  , cbKeyMouse = const (return st)
  }

instance Monad Callbacks where
  a >>= f =
    Callbacks
      (      cbSetup    a   >>=      cbSetup      . f)
      (      cbDisplay  a   >>=      cbDisplay    . f)
      (      cbIdle     a   >>=      cbIdle       . f)
      (\p -> cbReshape  a p >>= flip cbReshape  p . f)
      (\p -> cbKeyMouse a p >>= flip cbKeyMouse p . f)
  return x = let r = return x in Callbacks r r r (const r) (const r)

-------------------------------------------------------------------------------

withVar :: IORef a -> (a -> IO a) -> IO ()
withVar v c = get v >>= c >>= (v $=)

setup :: Aspect st -> IO ()
setup aspects = do

  -- Run all aspect setup functions.
  state <- cbSetup $ aspects (error "gl: do not eval aspect input in cbSetup")
  var <- newIORef state

  displayCallback       $=                   withVar var (      cbDisplay . aspects)
  reshapeCallback       $= Just (\sz ->      withVar var (\p -> cbReshape (aspects p) sz))
  idleCallback          $= Just (            withVar var (      cbIdle . aspects))
  keyboardMouseCallback $= Just (\k s m p -> withVar var (\q -> cbKeyMouse (aspects q) (k, s, m, p)))

  -- Start the engine.
  mainLoop

