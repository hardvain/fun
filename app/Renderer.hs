module Renderer where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Control.Monad
import Buffer 
import Program

data Mesh = Mesh {
  positionBufferObject :: GL.BufferObject,
  positions :: [GL.Vertex4  GL.GLfloat],
  colorBufferObject :: GL.BufferObject,
  colors :: [GL.Vertex4  GL.GLfloat],
  vao :: GL.VertexArrayObject
}

data RenderHint = RenderHint {
  primitiveMode :: GL.PrimitiveMode,
  startIndex :: Int,
  numVertices :: Int
}

render :: Window -> Mesh -> RenderHint -> IO ()
render window mesh hint@(RenderHint mode startIndex numVertices) = do
  GL.clearColor $= Color4 0 0 0 1
  GL.clear [ColorBuffer]
  withVertexArrayObject (vao mesh) $ do
    drawArrays mode (fromIntegral startIndex) (fromIntegral numVertices)
  GLFW.swapBuffers window
  forever $ do
    GLFW.pollEvents
    render window mesh hint

