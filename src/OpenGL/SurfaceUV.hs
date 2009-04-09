{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module OpenGL.SurfaceUV where

import Control.Applicative
import Control.Monad.State hiding (sequence)
import Data.Foldable hiding (sum)
import Engine.Geometry
-- import Engine.Math
-- import Engine.Normal
import Engine.Surface
-- import Engine.Projection
import Engine.Range
import Engine.Sampling
import Engine.Storable
-- import Engine.Texture
-- import Engine.Transform hiding (flip)
import Engine.Vector
import Foreign
-- import OpenGL.Drawing
import Prelude hiding (foldl)
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT (($=))

render :: Surface3 a Float -> IO (ForeignPtr Float, Int)
render surf =
  do ptr <- mallocForeignPtrBytes (totalBytes surf)
     withForeignPtr ptr
       $ runPtr (aspectPoke_UV (\i -> (
         ($i) <$> sSurface surf 
       , ($i) <$> sNormals surf
       , ($i) <$> sColors  surf
       , ($i) <$> sCoords  surf
       )) (sSampling surf)) . castPtr
     return (ptr, numVertices surf)

storeInVBO :: GL.BufferObject -> ForeignPtr a -> GL.GLsizeiptr -> IO ()
storeInVBO obj ptr i = 
  do withForeignPtr ptr $ \p ->
       do GL.bufferData GL.ArrayBuffer $= (i, nullPtr, GL.StaticDraw)
          GL.bufferSubData GL.ArrayBuffer GL.WriteToBuffer 0 i p
     GL.bindBuffer GL.ArrayBuffer $= Just obj

renderToVBO :: Surface3 a Float -> GL.BufferObject -> IO (Int, Int, [Int])
renderToVBO surf obj =
  do (fptr, n) <- render surf
     storeInVBO obj fptr (fromIntegral (totalBytes surf))
     return (n, strideBytes surf, strideComponents surf)

display obj surf =
  do let s = fromIntegral (strideBytes surf)
         o = (\k -> nullPtr `plusPtr` (k * f)) <$> strideOffsets surf
         c = strideComponents surf
         f = sizeOf (0::Float)
         n = fromIntegral (numVertices surf)

     GL.bindBuffer GL.ArrayBuffer $= Just obj

     -- TODO: conditional based on Maybes
     GL.clientState GL.ColorArray         $= GL.Enabled
     GL.clientState GL.TextureCoordArray  $= GL.Enabled
     GL.clientState GL.NormalArray        $= GL.Enabled
     GL.clientState GL.VertexArray        $= GL.Enabled

     GL.arrayPointer GL.VertexArray       $= GL.VertexArrayDescriptor (c !! 0) GL.Float s (o !! 0)
     GL.arrayPointer GL.NormalArray       $= GL.VertexArrayDescriptor (c !! 1) GL.Float s (o !! 1)
     GL.arrayPointer GL.ColorArray        $= GL.VertexArrayDescriptor (c !! 2) GL.Float s (o !! 2)
     GL.arrayPointer GL.TextureCoordArray $= GL.VertexArrayDescriptor (c !! 3) GL.Float s (o !! 3)

     GL.drawArrays GL.TriangleStrip 0 n

numVertices :: Surface3 a b -> Int
numVertices surf = 2 * foldl (*) 1 (V2 (-1+) id <*> (length <$> sSampling surf))

totalBytes :: Storable b => Surface3 a b -> Int
totalBytes surf = (strideBytes surf * numVertices surf)

strideBytes :: Storable b => Surface3 a b -> Int
strideBytes surf = sum ([
       sizeOf (($ undefined) <$> sSurface surf)
  ,    sizeOf (($ undefined) <$> sNormals surf)
  ,    sizeOf (($ undefined) <$> sColors  surf)
  ]) + sizeOf (($ undefined) <$> sCoords  surf)

strideComponents :: Num t => Surface3 a b -> [t]
strideComponents surf = [
    maybe 0 (const 3) (sSurface surf)
  , maybe 0 (const 3) (sNormals surf)
  , maybe 0 (const 4) (sColors  surf)
  ] ++ map (const 2) (sCoords surf)

strideOffsets :: Num c => Surface3 a b -> [c]
strideOffsets surf = offs (strideComponents surf)
  where offs a = 0 : zipWith (+) (init a) (offs a)

aspectM_UV :: Monad m => (UV a -> m b) -> UV (Samples a) -> m [b]
aspectM_UV f (V2 iU iV) = sequence $
  do u0 :~: u1 <- segmented iU
     v         <- iV
     return (f (V2 u0 v) >> f (V2 u1 v))

aspectPoke_UV
  :: (MonadIO m, Storable b, MonadState (Ptr b) m)
  => (UV a -> b) -> UV (Samples a) -> m ()
aspectPoke_UV f s = aspectM_UV (pokeST . f) s >> return ()







































{--- Combine two render aspects.
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
-}




