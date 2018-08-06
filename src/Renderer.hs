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

{- | The main draw function that draws the current frame to opengl window -}
draw :: SceneGraph Renderable -- A scenegraph of renderable objects
        -> Window  -- The GLFW window which renderes opengl graphics
        -> Int  -- Current FrameNumber
        -> Int  -- StartTime of the application
        -> IO ()
draw sceneGraph  = drawLoop (fmap (unsafePerformIO . initializePipelineState) sceneGraph)

drawLoop :: SceneGraph RenderPipelineState -- A scenegraph with completeley filled pipeline state
            -> Window  -- The GLFW window which renderes opengl graphics
            -> Int  -- Current FrameNumber
            -> Int  -- StartTime of the application
            -> IO ()
drawLoop sceneGraph@(SceneGraph stateTree) window frameNumber startTime = do
  setClearColor $ GL.Color4 0 0 0 1
  millisElpased <- elapsedTimeFrom startTime
  apply (render frameNumber millisElpased) stateTree 
  GLFW.swapBuffers window
  forever $ do
    GLFW.pollEvents
    drawLoop sceneGraph window (frameNumber + 1) startTime

renderHint :: Mesh -> RenderHint
renderHint mesh = RenderHint GL.Triangles 0 (numVerticesToDraw mesh)
    
render :: FrameNumber -> MillisElapsed -> RenderPipelineState -> IO ()
render frameNumber millisElapsed state = do
  let meshObj = mesh state
  let transformation = transformAnimations frameNumber millisElapsed $ meshAnimations meshObj
  let translatedMatrix = T.modelMatrix transformation (M.modelMatrix state)
  P.setMVPMatrix translatedMatrix
  P.useProgram (program state)
  withVertexArrayObject (vao meshObj) $ do
    let (RenderHint mode startIndex numVertices) = renderHint meshObj
    GL.drawArrays mode (fromIntegral startIndex) (fromIntegral numVertices)