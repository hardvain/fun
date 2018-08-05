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
import Renderable
import Animation 
import Ease 
import Time 
import qualified Transformation as T
import Drawable 

setClearColor :: GL.Color4 Float -> IO ()
setClearColor color = do
  GL.clearColor GL.$= color
  GL.clear [GL.ColorBuffer]
  return ()

draw :: SceneGraph Renderable -> Window ->  Int -> Int -> IO ()
draw sceneGraph  = drawLoop (fmap (unsafePerformIO . initializePipelineState) sceneGraph)

drawLoop ::  SceneGraph RenderPipelineState -> Window -> Int -> Int -> IO ()
drawLoop sceneGraph@(SceneGraph tree) window frameNumber startTime = do
  setClearColor $ GL.Color4 0 0 0 1
  millisElpased <- elapsedTimeFrom startTime
  apply (render frameNumber millisElpased) tree 
  GLFW.swapBuffers window
  forever $ do
    GLFW.pollEvents
    drawLoop sceneGraph window (frameNumber + 1) startTime

renderHint :: Mesh -> RenderHint
renderHint mesh = RenderHint GL.Triangles 0 (numVerticesToDraw mesh)
    
render :: FrameNumber -> MillisElapsed -> RenderPipelineState -> IO ()
render frameNumber millisElpased state = do
  let meshObj = mesh state
  let anims = (meshAnimations meshObj)
  let transformations = fmap (getTransformation frameNumber millisElpased) anims
  let tran = mconcat transformations
  let translatedMatrix = T.modelMatrix tran (M.modelMatrix state)
  P.setMVPMatrix translatedMatrix
  P.useProgram (program state)
  withVertexArrayObject (vao meshObj) $ do
    let (RenderHint mode startIndex numVertices) = renderHint meshObj
    GL.drawArrays mode (fromIntegral startIndex) (fromIntegral numVertices)

