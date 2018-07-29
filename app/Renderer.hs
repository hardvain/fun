module Renderer where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Control.Monad
import Buffer 
import Program
import Shape 

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
createMesh (colorsData, positions) = do
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

draw :: Drawable -> Window -> IO()
draw drawable window = do
  let renderHint = RenderHint GL.Triangles 0 3
  mesh <- createMesh drawable
  render window mesh renderHint

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

