{-# LANGUAGE FlexibleContexts #-}
module Renderer where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Control.Monad
import Buffer 
import Program
import Shape 
import Ease 
import Time

data UniformData  = UniformData String (GL.Vector3 GL.GLfloat)  GL.UniformLocation

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
createMesh (colorsData, positions, _) = do
  vao <- createVertexArrayObject
  positionBufferObject <- withVertexArrayObject vao (createAndDescribeBuffer positions 0 4)
  colorBufferObject <- withVertexArrayObject vao (createAndDescribeBuffer colorsData 1 4)
  return Mesh {
    positionBufferObject = positionBufferObject,
    positions = positions,
    colors = colorsData,
    colorBufferObject = colorBufferObject,
    vao = vao
  }

setClearColor :: Color4 GL.GLfloat -> IO()
setClearColor color = do
  GL.clearColor $= Color4 0 0 0 1
  GL.clear [ColorBuffer]

setUniform :: Float -> UniformData ->  IO()
setUniform time (UniformData name datum@(GL.Vector3 a b c) location) = do
  GL.uniform location GL.$= (GL.Vector3 (a+time) (b+time) (c+time))
  return ()

setUniforms :: [UniformData] -> IO()
setUniforms = mapM_ (setUniform 0) 

draw :: [Drawable] -> Window -> [UniformData] -> Int -> Integer -> IO ()
draw drawables window uniforms frameNumber startTime = do
  setClearColor $ Color4 0 0 0 1
  currentTime <- timeInMillis
  let elapsedTime = currentTime - startTime
  let value = (fromIntegral (mod (fromIntegral elapsedTime) 1000)) * 0.001
  setUniforms uniforms
  renderDrawables window drawables
  GLFW.swapBuffers window
  forever $ do
    GLFW.pollEvents
    draw drawables window uniforms (frameNumber + 1) startTime

renderDrawables :: Window -> [Drawable] -> IO()
renderDrawables window = mapM_ (render window)

render :: Window -> Drawable -> IO ()
render window  drawable@(_,_, numVertices)  = do
  let renderHint = RenderHint GL.Triangles 0 numVertices
  mesh <- createMesh drawable
  renderMesh renderHint mesh

renderMesh :: RenderHint -> Mesh -> IO ()
renderMesh hint@(RenderHint mode startIndex numVertices) mesh = withVertexArrayObject (vao mesh) $ do
  drawArrays mode (fromIntegral startIndex) (fromIntegral numVertices)
