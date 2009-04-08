module Scenes.Effects where

import Graphics.Rendering.OpenGL

import Engine.Color
import Engine.Interpolation
import Engine.Primitives
import Engine.Texture
import Engine.Timer
import Engine.Vector
import Engine.Range
import OpenGL.Drawing
import OpenGL.SurfaceUV

--------[ nice default settings ]----------------------------------------------

rotatingScene :: Double -> IO ()
rotatingScene s = do
  rotate 20 $ Vector3 1 0 (0::Float)
  360 `within` (seconds s) $
    flip rotate (Vector3 0.2 1 0.3)

--------[ motion blur ]--------------------------------------------------------

-- Add motion blur to a display callback function.
motionBlur :: (Float, Float, Float) -> IO a -> IO ()
motionBlur (mul, acc, ret) callback = do
  accum Mult mul
  callback
  accum Accum acc
  accum Return ret

lightMotionBlur :: IO a -> IO ()
lightMotionBlur = motionBlur (0.90, 0.1, 1.1)

heavyMotionBlur :: IO a -> IO ()
heavyMotionBlur = motionBlur (0.95, 0.05, 1.2)

--------[ reflection/wet floor ]-----------------------------------------------

reflectiveSurface :: IO a -> (TextureUV -> IO a2) -> IO a1 -> IO b -> IO b 
reflectiveSurface flor floorNoTx ref scene = do

  -- Save floor mask in stencil buffer.
  withNoDepth $ do
    withNoColor $ do
      stencilTest $= Enabled
      stencilFunc $= (Always, 1, 1)
      stencilOp   $= (OpKeep, OpKeep, OpReplace)
      flor
    stencilFunc $= (Equal, 1, 1)
    stencilOp   $= (OpKeep, OpKeep, OpKeep)

  -- Draw reflected scene masked on the floor.
  preservingMatrix ref
  stencilTest $= Disabled

  -- With reversed z-buffering draw solid black floor.
  depthFunc $= Just Greater
  floorNoTx $ solid black

  -- Draw the actual floor overwriting the solid black one.
  depthFunc $= Just Lequal
  flor

  -- Draw the actual scene.
  depthFunc $= Just Less
  scene

-- A sexy default for a slap wet floor effect.
wetFloor :: IO a -> IO b -> IO b
wetFloor ref scene = reflectiveSurface (flor tex) flor (scale 1 (-1::Float) 1 >> ref) scene
  where
    flor = renderTri . surfaceUV_VVC (tubeUV 0 (0 :~: 7)) (gradientUV (V2 16 1))
    tex  = vGradientUV (setAlpha (0 :: Float) gray :~: black)

chemicalPool :: IO a -> IO b -> IO b
chemicalPool ref scene = reflectiveSurface (flor tex) flor (ref) scene
  where
    flor = renderTri . surfaceUV_VVC (tubeUV 0 (0 :~: 7)) (gradientUV (V2 16 1))
    tex  = vGradientUV (V4 0.6 1 0 0 :~: black)

