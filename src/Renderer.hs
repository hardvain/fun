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

data UniformData  a = UniformData String a GL.UniformLocation

data Mesh = Mesh {
  positionBufferObject :: GL.BufferObject,
  positions :: [GL.Vertex4 Float],
  colorBufferObject :: GL.BufferObject,
  colors :: [Color4 Float],
  vao :: GL.VertexArrayObject
}

data RenderHint = RenderHint {
  primitiveMode :: GL.PrimitiveMode,
  startIndex :: Int,
  numVertices :: Int
}

createMesh :: Drawable -> IO Mesh
createMesh (Drawable positions colorsData _) = do
  withNewVertexArrayObject $ \vao -> do
    positionBufferObject <- createAndDescribeBuffer positions 0 4
    colorBufferObject <- createAndDescribeBuffer colorsData 1 4
    return $ Mesh positionBufferObject positions colorBufferObject colorsData  vao

setUniform :: (Uniform a) => Float -> UniformData a ->  IO ()
setUniform time (UniformData name datum location) = 
  GL.uniform location GL.$= datum
  
setClearColor :: Color4 Float -> IO ()
setClearColor color = do
  GL.clearColor $= color
  GL.clear [ColorBuffer]
  return ()

draw :: (Uniform a) => SceneGraph -> Window -> [UniformData a] -> Int -> Integer -> IO ()
draw sceneGraph window uniforms frameNumber startTime = do
  setClearColor $ Color4 0 0 0 1
  mapM_ (setUniform 0) uniforms
  renderSceneGraph window sceneGraph
  GLFW.swapBuffers window
  forever $ do
    GLFW.pollEvents
    draw sceneGraph window uniforms (frameNumber + 1) startTime

render :: Window -> Renderable -> IO ()
render window (Renderable  drawable@(Drawable _ _ numVertices) _ _) = do
  let renderHint = RenderHint GL.Triangles 0 numVertices
  mesh <- createMesh drawable
  renderMesh renderHint mesh

renderMesh :: RenderHint -> Mesh -> IO ()
renderMesh (RenderHint mode startIndex numVertices) mesh = withVertexArrayObject (vao mesh) $ do
  drawArrays mode (fromIntegral startIndex) (fromIntegral numVertices)

renderSceneGraph :: Window -> SceneGraph -> IO ()
renderSceneGraph window (SceneGraph tree) = renderTree window tree

renderTree :: Window -> Tree Renderable -> IO ()
renderTree window Empty = return ()
renderTree window (Node renderable trees) = do
  render window renderable
  mapM_ (renderTree window) trees
