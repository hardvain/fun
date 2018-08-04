{-# LANGUAGE FlexibleContexts #-}
module Renderer where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Control.Monad
import Buffer 
import Program
import Shape 
import Data.Ratio
import Ease 
import Time
import AST
import Matrix as M
import qualified Data.Time.Clock.POSIX as Time
import System.IO.Unsafe
import Program 
import Data.Matrix
import Mesh
import Drawable
import Renderable
import SceneGraph

setUniform :: Mesh -> IO ()
setUniform mesh = do
  let matrix = (mvpMatrix . renderable) mesh
  prog <- defaultProgram
  transformLocation <- GL.uniformLocation (glProgram prog) "transform"
  datum <- GL.newMatrix GL.ColumnMajor (toList matrix) :: IO (GL.GLmatrix GL.GLfloat)
  GL.uniform transformLocation GL.$= datum
  
setClearColor :: Color4 Float -> IO ()
setClearColor color = do
  GL.clearColor $= color
  GL.clear [ColorBuffer]
  return ()

draw ::SceneGraph Renderable -> Window ->  Int -> Integer -> IO ()
draw sceneGraph = drawLoop (populateMeshes sceneGraph) 

drawLoop ::  SceneGraph Mesh -> Window -> Int -> Integer -> IO ()
drawLoop sceneGraph window frameNumber startTime = do
  setClearColor $ Color4 0 0 0 1
  _ <- pure $ fmap (setUniform >> renderMesh) sceneGraph
  GLFW.swapBuffers window
  forever $ do
    GLFW.pollEvents
    drawLoop sceneGraph window (frameNumber + 1) startTime


renderHint :: Mesh -> RenderHint
renderHint mesh = RenderHint GL.Triangles 0 (numberOfVertices . drawable . renderable $ mesh)

renderMesh :: Mesh -> IO ()
renderMesh mesh = withVertexArrayObject (vao mesh) $ do
  let (RenderHint mode startIndex numVertices) = renderHint mesh
  drawArrays mode (fromIntegral startIndex) (fromIntegral numVertices)
