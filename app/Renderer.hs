module Renderer (
  render,
  Descriptor(..)
) where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Control.Monad

data Descriptor = Descriptor VertexArrayObject ArrayIndex NumArrayIndices

render :: Window -> Descriptor -> IO ()
render window descriptor@(Descriptor vertexObject firstIndex numVertices) = do
  GL.clearColor $= Color4 1 0 0 1
  GL.clear [ColorBuffer]
  bindVertexArrayObject $= Just vertexObject
  drawArrays Triangles firstIndex numVertices
  GLFW.swapBuffers window
  forever $ do
    GLFW.pollEvents
    render window descriptor