{-# LANGUAGE FlexibleContexts  #-}
module Engine.Storable where

import Control.Applicative
import Control.Monad.State
import Foreign

instance (Storable a, Storable b) => Storable (a, b) where
  sizeOf (a, b) = sizeOf a + sizeOf b
  alignment _   = alignment (0::Int)
  peek p        = (,) <$> peekElemOff (castPtr p) 0 <*> peekElemOff (castPtr p) 1
  poke p (a, b) = poke (castPtr p) a >> poke (castPtr (p `plusPtr` sizeOf a)) b

instance (Storable a, Storable b, Storable c) => Storable (a, b, c) where
  sizeOf (a, b, c) = sizeOf (a, (b, c))
  alignment _      = alignment (0::Int)
  peek p           = (\(a, (b, c)) -> (a, b, c)) <$> peek (castPtr p)
  poke p (a, b, c) = poke (castPtr p) (a, (b, c))

instance (Storable a, Storable b, Storable c, Storable d) => Storable (a, b, c, d) where
  sizeOf (a, b, c, d) = sizeOf ((a, b), (c, d))
  alignment _         = alignment (0::Int)
  peek p              = (\((a, b), (c, d)) -> (a, b, c, d)) <$> peek (castPtr p)
  poke p (a, b, c, d) = poke (castPtr p) ((a, b), (c, d))

instance Storable a => Storable (Maybe a) where
  sizeOf (Just a) = sizeOf a
  sizeOf Nothing  = 0
  alignment a     = alignment a
  peek p          = error "Can only poke Maybe"
  poke p (Just a) = poke (castPtr p) a
  poke p Nothing  = return ()

instance Storable a => Storable [a] where
  sizeOf a        = sum (sizeOf <$> a)
  alignment a     = alignment a
  peek p          = error "Can only poke []"
  poke p []       = return ()
  poke p (x:xs)   = poke (castPtr p) x >> poke (castPtr (p `plusPtr` sizeOf x)) xs

pokeST :: (MonadIO m, Storable a, MonadState (Ptr a) m) => a -> m ()
pokeST a = get >>= (liftIO . flip poke a) >> modify (`plusPtr` sizeOf a)

runPtr :: Storable a => StateT (Ptr a) IO b -> Ptr a -> IO ()
runPtr st p = execStateT st p >> return ()

