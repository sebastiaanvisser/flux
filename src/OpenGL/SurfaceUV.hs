{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module OpenGL.SurfaceUV where

import Control.Applicative
import Control.Monad.State hiding (sequence)
import Data.Foldable
import Engine.Geometry
import Engine.Math
import Engine.Normal
import Engine.Projection
import Engine.Range
import Engine.Storable
import Engine.Sampling
import Engine.Texture
import Engine.Transform hiding (flip)
import Engine.Vector
import Foreign
import OpenGL.Drawing
import Prelude hiding (foldl)
import qualified Graphics.Rendering.OpenGL as GL

-- Combine two render aspects.
combine :: Render -> Render -> Render
combine = zipWith (zipWith (\(a,b) (c,d) -> (a >> c, b >> d)))

aspect_UV :: (UV Float -> a) -> Sampler_UV Float -> [[(a, a)]]
aspect_UV f (V2 iU iV) = map (\(u0 :~: u1) -> [ (f $ V2 u0 v, f $ V2 u1 v) | v <- iV ]) (segmented iU)

-------------------------------------------------------------------------------

-- Some helper converters to keep the code below clean.

v3ToVertex3 :: Vector3 t -> GL.Vertex3 t
v3ToVertex3 (V3 x y z) = GL.Vertex3 x y z

v3ToNormal3 :: Vector3 t -> GL.Normal3 t
v3ToNormal3 (V3 x y z) = GL.Normal3 x y z

v4ToColor4 :: Vector4 t -> GL.Color4 t
v4ToColor4 (V4 r g b a) = GL.Color4  r g b a

v2ToTexCoord2 :: Vector2 t -> GL.TexCoord2 t
v2ToTexCoord2 (V2 u v) = GL.TexCoord2 u v

-- Different aspect renderers for vertex projections, texture coordinates,
-- texture mapping and normal projection.
vertex    :: Proj_UV_Vertex3 Float Float -> Sampler_UV Float -> Render
texCoord  ::                                Sampler_UV Float -> Render
mTexCoord :: Int                         -> Sampler_UV Float -> Render
color     :: Texture_UV      Float Float -> Sampler_UV Float -> Render
normal    :: Proj_UV_Normal3 Float Float -> Sampler_UV Float -> Render
normal'   :: Proj_UV_Vertex3 Float Float -> 
             Proj_UV_Normal3 Float Float -> Sampler_UV Float -> Render

vertex   vp = aspect_UV (GL.vertex          . v3ToVertex3 . vp)
color    tx = aspect_UV (GL.color           . v4ToColor4 . tx . fmap almost)
texCoord    = aspect_UV (GL.texCoord        . v2ToTexCoord2)
mTexCoord n = aspect_UV (GL.multiTexCoord m . v2ToTexCoord2) where m = GL.TextureUnit (fromIntegral n)
normal   np = aspect_UV (GL.normal          . v3ToNormal3 . np)

normal' vp np = aspect_UV $ \uv ->
  do let a = vp uv
         b = trans a (np uv)
     GL.color (GL.Color3 1 (0::Float) 0) >> GL.vertex (v3ToVertex3 a)
     GL.color (GL.Color3 0 (0::Float) 1) >> GL.vertex (v3ToVertex3 b)

-- Renderers that combine different aspects.

surface_UV_V     :: Proj_UV_Vertex3 Float Float ->                                                          Sampler_UV Float -> Render
surface_UV_VT    :: Proj_UV_Vertex3 Float Float ->                                                          Sampler_UV Float -> Render
surface_UV_VTC   :: Proj_UV_Vertex3 Float Float -> Texture_UV Float Float ->                                Sampler_UV Float -> Render
surface_UV_VTCN  :: Proj_UV_Vertex3 Float Float -> Texture_UV Float Float -> Proj_UV_Normal3 Float Float -> Sampler_UV Float -> Render
surface_UV_VTN   :: Proj_UV_Vertex3 Float Float ->                           Proj_UV_Normal3 Float Float -> Sampler_UV Float -> Render
surface_UV_VTT   :: Proj_UV_Vertex3 Float Float ->                                                          Sampler_UV Float -> Render
surface_UV_VTTC  :: Proj_UV_Vertex3 Float Float -> Texture_UV Float Float ->                                Sampler_UV Float -> Render
surface_UV_VTTCN :: Proj_UV_Vertex3 Float Float -> Texture_UV Float Float -> Proj_UV_Normal3 Float Float -> Sampler_UV Float -> Render

surface_UV_V     vp       ip = vertex   vp ip
surface_UV_VT    vp       ip = texCoord    ip `combine` surface_UV_V   vp ip
surface_UV_VTC   vp tx    ip = color    tx ip `combine` surface_UV_VT  vp ip
surface_UV_VTCN  vp tx np ip = normal   np ip `combine` surface_UV_VTC vp tx ip 
surface_UV_VTN   vp    np ip = normal   np ip `combine` surface_UV_VT  vp ip
surface_UV_VTT   vp       ip = mTexCoord 0 ip `combine` mTexCoord 1        ip `combine` surface_UV_V   vp ip
surface_UV_VTTC  vp tx    ip = color    tx ip `combine` surface_UV_VTT  vp ip
surface_UV_VTTCN vp tx np ip = normal   np ip `combine` surface_UV_VTTC vp tx ip 

normal_UV
  :: Proj_UV_Vertex3 Float Float
  -> Proj_UV_Normal3 Float Float
  -> Sampler_UV Float
  -> Render
normal_UV vp np ip = normal' vp np ip












aspectM_UV :: Monad m => (UV a -> m b) -> UV (Samples a) -> m [b]
aspectM_UV f (V2 iU iV) = sequence $
  do u0 :~: u1 <- segmented iU
     v         <- iV
     return (f (V2 u0 v) >> f (V2 u1 v))

pokeST :: (MonadIO m, Storable a, MonadState (Ptr a) m) => a -> m ()
pokeST a = get >>= (liftIO . flip poke a) >> modify (`plusPtr` sizeOf a)

aspectPoke_UV
  :: (MonadIO m, Storable b, MonadState (Ptr b) m)
  => (UV a -> b) -> UV (Samples a) -> m ()
aspectPoke_UV f s = aspectM_UV (pokeST . f) s >> return ()

sf_UV_VTCN :: Storable b => (Proj_UV a b) -> UV (Samples a) -> Ptr b -> IO ()
sf_UV_VTCN vp ip p = execStateT (aspectPoke_UV vp ip) p >> return ()

sf_UV_VTCN_alloc
  :: Proj_UV_Vertex3 Float Float
  -> Texture_UV Float Float
  -> Proj_UV_Normal3 Float Float
  -> Sampler_UV Float
  -> IO (ForeignPtr Float, Int)

sf_UV_VTCN_alloc vp cp np ip =
  do let n = 2 * foldl (*) 1 (V2 (-1+) id <*> (length <$> ip))
     ptr <- mallocForeignPtrArray ((4+2+3+3) * n * sizeOf (undefined :: Float)) :: IO (ForeignPtr Float)
     withForeignPtr ptr $ sf_UV_VTCN (\i -> (cp i, i, np i, vp i)) ip . castPtr
     return (ptr, n)

