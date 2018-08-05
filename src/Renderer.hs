{-# LANGUAGE FlexibleContexts #-}
module Renderer where

import qualified Graphics.Rendering.OpenGL as GL
import qualified OpenGL.Program as P
import Graphics.UI.GLFW as GLFW
import Control.Monad
import OpenGL.Buffer 
import System.IO.Unsafe
import Mesh as M
import SceneGraph 
import Renderable as R
import Animation 
import Ease 
import Time 

setClearColor :: GL.Color4 Float -> IO ()
setClearColor color = do
  GL.clearColor GL.$= color
  GL.clear [GL.ColorBuffer]
  return ()

setMVPMatrix :: Mesh -> IO ()
setMVPMatrix mesh = P.setMVPMatrix (mvpMatrix . renderable $ mesh)
  
draw :: SceneGraph Renderable -> Window ->  Int -> Integer -> IO ()
draw sceneGraph  = drawLoop (fmap (unsafePerformIO . initializePipelineState) sceneGraph)

drawLoop ::  SceneGraph RenderPipelineState -> Window -> Int -> Integer -> IO ()
drawLoop sceneGraph@(SceneGraph tree) window frameNumber startTime = do
  setClearColor $ GL.Color4 0 0 0 1
  currentTime <- timeInMillis
  let millisElpased = fromIntegral (currentTime - startTime)
  apply (render frameNumber millisElpased) tree 
  GLFW.swapBuffers window
  forever $ do
    GLFW.pollEvents
    drawLoop sceneGraph window (frameNumber + 1) startTime

renderHint :: Mesh -> RenderHint
renderHint mesh = RenderHint GL.Triangles 0 (numberOfVertices . drawable . renderable $ mesh)
    
render :: FrameNumber -> MillisElapsed -> RenderPipelineState -> IO ()
render frameNumber millisElpased state = do
  let meshObj = mesh state
  let anims = (animations . renderable $ meshObj)
  let transformations = map (processAnimation frameNumber millisElpased) anims
  let tran = mconcat transformations
  _ <- putStrLn (show tran)
  P.setMVPMatrix (R.modelMatrix tran (M.modelMatrix state))
  P.useProgram (program state)
  withVertexArrayObject (vao meshObj) $ do
    let (RenderHint mode startIndex numVertices) = renderHint meshObj
    GL.drawArrays mode (fromIntegral startIndex) (fromIntegral numVertices)

processAnimation :: FrameNumber -> MillisElapsed -> Animation -> Transformation
processAnimation frameNumber millisElpased anim = getTransformationForTarget (target anim) $ runAnimation anim  frameNumber millisElpased

getTransformationForTarget :: AnimationTarget -> (Float -> Transformation)
getTransformationForTarget PositionX = (\v -> Transformation (Position v 0 0) defaultRotation defaultScale)
getTransformationForTarget PositionY = (\v -> Transformation (Position 0 v 0) defaultRotation defaultScale)
getTransformationForTarget PositionZ = (\v -> Transformation (Position 0 0 v) defaultRotation defaultScale)
getTransformationForTarget ScaleX = (\v -> Transformation defaultPosition defaultRotation (Scale v 1 1))
getTransformationForTarget ScaleY = (\v -> Transformation defaultPosition defaultRotation (Scale 1 v 1))
getTransformationForTarget ScaleZ = (\v -> Transformation defaultPosition defaultRotation (Scale 1 1 v))
getTransformationForTarget RotationX = (\v -> Transformation defaultPosition (Rotation v 0 0) defaultScale)
getTransformationForTarget RotationY = (\v -> Transformation defaultPosition (Rotation 0 v 0) defaultScale)
getTransformationForTarget RotationZ = (\v -> Transformation defaultPosition (Rotation 0 0 v) defaultScale)
