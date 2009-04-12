{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module OpenGL.SurfaceUV where

import Control.Applicative
import Control.Monad.State hiding (sequence)
import Data.Foldable hiding (sum, sequence_)
import Engine.Geometry
import Engine.Range
import Engine.Sampling
import Engine.Storable
import Engine.Surface
import Engine.Vector
import Foreign
import Prelude hiding (foldl)
import System.IO
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))

data Attribute  = Vertex | Normal | Color | Coordinate
  deriving Show

type Components = Int
type Attributes = [(Attribute, Components)]

compToInt :: Attribute -> Int
compToInt Vertex     = 0
compToInt Normal     = 1
compToInt Color      = 2
compToInt Coordinate = 3

intToComp :: Int -> Attribute
intToComp 0 = Vertex    
intToComp 1 = Normal    
intToComp 2 = Color     
intToComp _ = Coordinate

instance Storable Attribute where
  sizeOf    _ = sizeOf (0::Int)
  alignment _ = alignment (0::Int)
  peek p      = intToComp <$> peek (castPtr p)
  poke p c    = poke (castPtr p) (compToInt c)

data RawObject a =
  RawObject {
    roVertices   :: Int
  , roSizeOfUnit :: Int
  , roAttributes :: Attributes
  , roData       :: ForeignPtr a
  } deriving Show

-- Top level.

renderSurface3ToRawObject :: forall a b. Storable b => Surface3 a b -> IO (RawObject b)
renderSurface3ToRawObject surf@(Surface3 s n c d m) =
  do print (totalBytes surf)
     ptr <- mallocForeignPtrBytes (totalBytes surf)
     withForeignPtr ptr
       $ execPtr (aspectPoke_UV (\i -> (s <*> pure i, n <*> pure i, c <*> pure i, d <*> pure i)) m)
       . castPtr
     return $ RawObject
       (verticeCount surf)
       (sizeOf (undefined :: b))
       (attributes surf)
       ptr

readRawObjectFromFile :: FilePath -> IO (RawObject a)
readRawObjectFromFile fp =
  withBinaryFile fp ReadMode $ \h ->
    do v <- hGetStorable h
       s <- hGetStorable h
       l <- hGetStorable h
       a <- hGetList h l
       let t = v * s * sum (map snd a)
       p <- mallocForeignPtrBytes t
       withForeignPtr p $ flip (hGetBuf h) t
       return (RawObject v s a p)

writeRawObjectToFile :: FilePath -> RawObject a -> IO ()
writeRawObjectToFile fp (RawObject v s a p) = do
  print (v, s, a, p)
  withBinaryFile fp WriteMode $ \h ->
    do let t = v * s * sum (map snd a)
       hPutStorable h v
       hPutStorable h s
       hPutStorable h (length a)
       hPutList h a
       print t
       withForeignPtr p $ flip (hPutBuf h) t

renderSurface3ToFile :: Storable b => FilePath -> Surface3 a b -> IO ()
renderSurface3ToFile f = writeRawObjectToFile f <=< renderSurface3ToRawObject

storeFileInVBO :: FilePath -> GL.BufferObject -> IO (RawObject a)
storeFileInVBO f o = 
  do raw <- readRawObjectFromFile f
     storeRawObjectInVBO o raw
     return raw

renderSurface3ToVBO :: Storable b => GL.BufferObject -> Surface3 a b -> IO (RawObject b)
renderSurface3ToVBO obj surf = do
  raw <- renderSurface3ToRawObject surf
  storeRawObjectInVBO obj raw
  return raw

storeRawObjectInVBO :: GL.BufferObject -> RawObject b -> IO ()
storeRawObjectInVBO obj (RawObject v s a ptr) = 
  do let n = fromIntegral (v * s * sum (map snd a))
     GL.bindBuffer GL.ArrayBuffer $= Just obj
     withForeignPtr ptr $ \p ->
       do GL.bufferData GL.ArrayBuffer $= (n, nullPtr, GL.StaticDraw)
          GL.bufferSubData GL.ArrayBuffer GL.WriteToBuffer 0 n p

displayVBO :: GL.BufferObject -> RawObject Float -> IO ()
displayVBO obj (RawObject v s a _) =
  do let cmp = map snd a
         str = s * sum cmp
         typ = map (compToGL . fst) a
         k   = init . scanl1 (+) . (0:) $ cmp
         ofs = plusPtr nullPtr . (*s) <$> k
         atr = filter (\(_, c, _) -> c /= 0) $ zipWith3 (,,) typ cmp ofs
         on   (u, _, _) = GL.clientState  u $= GL.Enabled
         off  (u, _, _) = GL.clientState  u $= GL.Disabled
         plot (u, b, c) = GL.arrayPointer u $=
                            GL.VertexArrayDescriptor
                            (fromIntegral b) GL.Float (fromIntegral str) c

     GL.bindBuffer GL.ArrayBuffer $= Just obj
     sequence_ (map on atr)
     sequence_ (map plot atr)
     GL.drawArrays GL.TriangleStrip 0 (fromIntegral v)
     sequence_ (map off atr)

-- Helpers.

compToGL :: Attribute -> GL.ClientArrayType
compToGL Vertex     = GL.VertexArray
compToGL Normal     = GL.NormalArray
compToGL Color      = GL.ColorArray
compToGL Coordinate = GL.TextureCoordArray

-- toto: compute components from vector.
attributes :: Surface3 a b -> Attributes
attributes surf = [
    ((,) Vertex . maybe 0 (const 3) . sSurface) surf
  , ((,) Normal . maybe 0 (const 3) . sNormals) surf
  , ((,) Color  . maybe 0 (const 4) . sColors ) surf
  ] ++ map (const (Coordinate, 2)) (sCoords  surf)

verticeCount :: Surface3 a b -> Int
verticeCount surf = 2 * foldl (*) 1 (V2 (-1+) id <*> (length <$> sSampling surf))

strideBytes :: forall a b. Storable b => Surface3 a b -> Components
strideBytes = (* sizeOf (undefined::b)) . sum . map snd . attributes

totalBytes :: Storable b => Surface3 a b -> Int
totalBytes surf = (strideBytes surf * verticeCount surf)

aspectM_UV :: Monad m => (UV a -> m b) -> UV (Samples a) -> m [b]
aspectM_UV f (V2 iU iV) = sequence $
  do u0 :~: u1 <- segmented iU ; v <- iV
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




