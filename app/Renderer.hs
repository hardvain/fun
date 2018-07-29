module Renderer (
  render,
  Descriptor(..),
  Mesh(..)
) where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Control.Monad
import Buffer 
import Program
data Descriptor = Descriptor VertexArrayObject ArrayIndex NumArrayIndices


data Mesh = Mesh {
  positionBufferObject :: GL.BufferObject,
  positions :: [GL.Vertex3  GL.GLfloat],
  vao :: GL.VertexArrayObject
}


render :: Window -> Mesh -> IO ()
render window mesh = do
  GL.clearColor $= Color4 0 0.5 0.5 1
  GL.clear [ColorBuffer]
  withVertexArrayObject (vao mesh) $ do
    drawArrays Triangles 0 6
  GLFW.swapBuffers window
  forever $ do
    GLFW.pollEvents
    render window mesh

