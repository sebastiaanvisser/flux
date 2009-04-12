{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module Engine.Storable where

import Control.Applicative
import Control.Monad.State
import Foreign
import System.IO

-- Continuesly peeking and poking using the state monad.

peekST :: (MonadIO m, Storable b, MonadState (Ptr a) m) => m b
peekST = get >>= (liftIO . peek . castPtr) >>= \a -> modify (`plusPtr` sizeOf a) >> return a

peekListST :: (MonadIO m, Storable b, MonadState (Ptr a) m) => Int -> m [b]
peekListST n = sequence (replicate n peekST)

pokeST :: (MonadIO m, Storable b, MonadState (Ptr a) m) => b -> m ()
pokeST a = get >>= (liftIO . flip poke a . castPtr) >> modify (`plusPtr` sizeOf a)

pokeListST :: (MonadIO m, Storable b, MonadState (Ptr a) m) => [b] -> m ()
pokeListST xs = sequence_ (map pokeST xs)

execPtr :: StateT (Ptr a) IO b -> (Ptr a) -> IO ()
execPtr st p = execStateT st p >> return ()

runPtr :: StateT (Ptr a) IO b -> Ptr a -> IO b
runPtr st p = fst <$> runStateT st p

-- Some common Storable instances.

instance (Storable a, Storable b) => Storable (a, b) where
  sizeOf ~(a, b) = sizeOf a + sizeOf b
  alignment _    = alignment (0::Int)
  peek p         = (,) <$> peekElemOff (castPtr p) 0 <*> peekElemOff (castPtr p) 1
  poke p (a, b)  = poke (castPtr p) a >> poke (castPtr (p `plusPtr` sizeOf a)) b

instance (Storable a, Storable b, Storable c) => Storable (a, b, c) where
  sizeOf ~(a, b, c) = sizeOf (a, (b, c))
  alignment _       = alignment (0::Int)
  peek p            = (\(a, (b, c)) -> (a, b, c)) <$> peek (castPtr p)
  poke p (a, b, c)  = poke (castPtr p) (a, (b, c))

instance (Storable a, Storable b, Storable c, Storable d) => Storable (a, b, c, d) where
  sizeOf ~(a, b, c, d) = sizeOf ((a, b), (c, d))
  alignment _          = alignment (0::Int)
  peek p               = (\((a, b), (c, d)) -> (a, b, c, d)) <$> peek (castPtr p)
  poke p (a, b, c, d)  = poke (castPtr p) ((a, b), (c, d))

instance Storable a => Storable (Maybe a) where
  sizeOf Nothing   = 0
  sizeOf ~(Just a) = sizeOf a
  alignment a      = alignment a
  peek _           = error "Can only poke Maybe"
  poke _ Nothing   = return ()
  poke p (Just a)  = poke (castPtr p) a

instance Storable a => Storable [a] where
  sizeOf a        = sum (sizeOf <$> a)
  alignment a     = alignment a
  peek _          = error "Can only poke []"
  poke _ []       = return ()
  poke p (x:xs)   = poke (castPtr p) x >> poke (castPtr (p `plusPtr` sizeOf x)) xs

peekList :: Storable a => Ptr [a] -> Int -> IO [a]
peekList p n = runPtr (peekListST n) (castPtr p)

pokeList :: Storable b => Ptr a -> [b] -> IO ()
pokeList p xs = runPtr (pokeListST xs) (castPtr p)

-- Using storable for binary IO.

hBufferedPut :: Handle -> (Ptr a -> a -> IO ()) -> Int -> a -> IO ()
hBufferedPut h poker s a =
  do f <- mallocForeignPtrBytes s
     withForeignPtr f $ \p ->
       do poker p a
          hPutBuf h p s

hBufferedGet :: Handle -> (Ptr a -> IO a) -> Int -> IO a
hBufferedGet h peeker s =
  do f <- mallocForeignPtrBytes s
     withForeignPtr f $ \p ->
       do hGetBuf h p s
          peeker p

hGetStorable :: forall a. Storable a => Handle -> IO a
hGetStorable h = hBufferedGet h peek (sizeOf (undefined :: a))

hPutStorable :: forall a. Storable a => Handle -> a -> IO ()
hPutStorable h a = hBufferedPut h poke (sizeOf (undefined :: a)) a

hGetList :: forall a. Storable a => Handle -> Int -> IO [a]
hGetList h n = hBufferedGet h (flip peekList n) (sizeOf (undefined :: a) * n)

hPutList :: forall a. Storable a => Handle -> [a] -> IO ()
hPutList h xs = hBufferedPut h pokeList (sizeOf (undefined :: a) * (length xs)) xs

