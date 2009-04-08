module OpenGL.Drawing where

import Graphics.Rendering.OpenGL

type Strip  = [(IO (), IO ())]
type Render = [Strip]

-------------------------------------------------------------------------------

-- Finalize a set of render aspects into one renderer.

render :: PrimitiveMode -> Render -> IO ()
render p = mapM_ (renderPrimitive p . sequence_ . map (\(a, b) -> a >> b))

renderTri, renderQuad, renderWire :: Render -> IO ()
renderTri  = render TriangleStrip
renderQuad = render Quads
renderWire = render Lines

-------------------------------------------------------------------------------

withAction :: Monad m => m a -> m b -> m c -> m b
withAction a c b = a >> b >> c

-- Temporarily turn off color rendering.
withNoColor :: IO a -> IO ()
withNoColor = withAction
  (colorMask $= Color4 Disabled Disabled Disabled Disabled)
  (colorMask $= Color4 Enabled  Enabled  Enabled  Enabled)

-- Temporarily turn off depth masking.
withNoDepth :: IO a -> IO ()
withNoDepth = withAction
  (depthMask $= Disabled)
  (depthMask $= Enabled)

withTexture2D :: TextureObject -> IO a -> IO ()
withTexture2D f = withAction
  (texture Texture2D $= Enabled >>
   textureBinding Texture2D $= Just f)
  (texture Texture2D $= Disabled)

withNormalMap2D
  :: (GLuint, GLuint)
  -> (GLuint, GLuint)
  -> IO a -> IO a
withNormalMap2D (nu, nm) (du, dm) action =
  do activeTexture $= TextureUnit nu
     texture Texture2D $= Enabled
     textureBinding Texture2D $= Just (TextureObject nm)
     textureFunction $= Combine
     combineRGB $= Dot3RGB
     argRGB Arg0 $= Arg SrcColor CurrentUnit
     argRGB Arg1 $= Arg SrcColor PrimaryColor

     activeTexture $= TextureUnit du
     texture Texture2D $= Enabled
     textureBinding Texture2D $= Just (TextureObject dm)
     textureFunction $= Combine
     combineRGB $= Modulate'
     argRGB Arg0 $= Arg SrcColor Previous
     argRGB Arg1 $= Arg SrcColor CurrentUnit
     action

