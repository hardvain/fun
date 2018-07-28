module Renderer (
  render,
  Descriptor(..)
) where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Control.Monad
import Buffer 
data Descriptor = Descriptor VertexArrayObject ArrayIndex NumArrayIndices

render :: Window -> Descriptor -> IO ()
render window descriptor@(Descriptor vertexObject firstIndex numVertices) = do
  GL.clearColor $= Color4 0 0.5 0.5 1
  GL.clear [ColorBuffer]
  withVertexObject vertexObject $ do
    drawArrays Triangles firstIndex numVertices
  GLFW.swapBuffers window
  forever $ do
    GLFW.pollEvents
    render window descriptor