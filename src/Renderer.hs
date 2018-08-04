{-# LANGUAGE FlexibleContexts #-}
module Renderer where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Control.Monad
import Buffer 
import qualified Program as P
import AST
import Matrix as M
import Data.Matrix
import Drawable
import Mesh
import Renderable
import SceneGraph
  
setClearColor :: Color4 Float -> IO ()
setClearColor color = do
  GL.clearColor $= color
  GL.clear [ColorBuffer]
  return ()

draw :: SceneGraph Renderable -> Window ->  Int -> Integer -> IO ()
draw sceneGraph = drawLoop (populateMeshes sceneGraph) 

drawLoop :: SceneGraph Mesh -> Window -> Int -> Integer -> IO ()
drawLoop sceneGraph window frameNumber startTime = do
  setClearColor $ Color4 0 0 0 1
  _ <- pure $ fmap (setMVPMatrix >> render) sceneGraph
  GLFW.swapBuffers window
  forever $ do
    GLFW.pollEvents
    drawLoop sceneGraph window (frameNumber + 1) startTime

setMVPMatrix :: Mesh -> IO ()
setMVPMatrix mesh = P.setMVPMatrix (mvpMatrix . renderable $ mesh)

renderHint :: Mesh -> RenderHint
renderHint mesh = RenderHint GL.Triangles 0 (numberOfVertices . drawable . renderable $ mesh)

render :: Mesh -> IO ()
render mesh = withVertexArrayObject (vao mesh) $ do
  let (RenderHint mode startIndex numVertices) = renderHint mesh
  drawArrays mode (fromIntegral startIndex) (fromIntegral numVertices)
