{-# LANGUAGE FlexibleContexts #-}
module Renderer where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Control.Monad
import Buffer 
import Program
import Shape 
import Data.Time.Clock.POSIX
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
  positionBufferObject <- withVertexArrayObject vao $ do
    positionBufferObject <- createArrayBuffer positions
    let positionDescriptor = VertexAttributeDescriptor {
      attributeLocation = 0,
      dimension = 4
    }
    describeAttribute positionDescriptor
    return positionBufferObject
  colorBufferObject <- withVertexArrayObject vao $ do
    colorBufferObject <- createArrayBuffer colorsData
    let colorDescriptor = VertexAttributeDescriptor {
      attributeLocation = 1,
      dimension = 4
    }
    describeAttribute colorDescriptor
    return colorBufferObject
  return Mesh {
    positionBufferObject = positionBufferObject,
    positions = positions,
    colors = colorsData,
    colorBufferObject = colorBufferObject,
    vao = vao
  }

setUniform :: Float -> UniformData ->  IO()
setUniform time (UniformData name datum@(GL.Vector3 a b c) location) = do
  GL.uniform location GL.$= (GL.Vector3 (a+time) (b+time) (c+time))
  return ()

draw :: [Drawable] -> Window -> [UniformData] -> IO()
draw drawables window uniforms = draw' drawables window uniforms 0
  where
    draw' drawables window uniforms frameCount = do
      GL.clearColor $= Color4 0 0 0 1
      GL.clear [ColorBuffer]
      mapM_ (setUniform 0) uniforms
      mapM_ (render window) drawables
      GLFW.swapBuffers window
      _ <- putStrLn (show frameCount)
      forever $ do
        GLFW.pollEvents
        draw' drawables window uniforms (frameCount + 1)

render :: Window -> Drawable -> IO ()
render window  drawable@(_,_, numVertices)  = do
  let renderHint = RenderHint GL.Triangles 0 numVertices
  mesh <- createMesh drawable
  renderMesh renderHint mesh

renderMesh :: RenderHint -> Mesh -> IO ()
renderMesh hint@(RenderHint mode startIndex numVertices) mesh = withVertexArrayObject (vao mesh) $ do
  drawArrays mode (fromIntegral startIndex) (fromIntegral numVertices)
