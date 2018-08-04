{-# LANGUAGE FlexibleContexts #-}
module Renderer where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Control.Monad
import Buffer 
import qualified Program as P
import Shape 
import Data.Ratio
import Ease 
import Time
import AST
import Matrix as M
import qualified Data.Time.Clock.POSIX as Time
import System.IO.Unsafe
import Data.Matrix
import Mesh

setClearColor :: Color4 Float -> IO ()
setClearColor color = do
  GL.clearColor $= color
  GL.clear [ColorBuffer]
  return ()

setMVPMatrix :: Mesh -> IO ()
setMVPMatrix mesh = P.setMVPMatrix (mvpMatrix . renderable $ mesh)
  
draw :: SceneGraph Renderable -> Window ->  Int -> Integer -> IO ()
draw sceneGraph  = drawLoop (populateMeshes sceneGraph)

drawLoop ::  SceneGraph Mesh -> Window -> Int -> Integer -> IO ()
drawLoop sceneGraph@(SceneGraph tree) window frameNumber startTime = do
  setClearColor $ Color4 0 0 0 1
  renderTree tree
  GLFW.swapBuffers window
  forever $ do
    GLFW.pollEvents
    drawLoop sceneGraph window (frameNumber + 1) startTime

renderHint :: Mesh -> RenderHint
renderHint mesh = RenderHint GL.Triangles 0 (numberOfVertices . drawable . renderable $ mesh)
    
render :: Mesh -> IO ()
render mesh = do
  _ <- setMVPMatrix mesh
  P.useProgram (meshProgram mesh)
  withVertexArrayObject (vao mesh) $ do
    let (RenderHint mode startIndex numVertices) = renderHint mesh
    drawArrays mode (fromIntegral startIndex) (fromIntegral numVertices)

renderTree :: Tree Mesh -> IO ()
renderTree Empty = return ()
renderTree (Node mesh trees) = do
  render mesh
  mapM_ (renderTree) trees
