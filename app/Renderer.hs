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

draw :: [Drawable] -> Window -> IO()
draw drawables window = do
  GL.clearColor $= Color4 0 0 0 1
  GL.clear [ColorBuffer]
  let renderHint = RenderHint GL.Triangles 0 3
  mapM_ (render window renderHint) drawables
  GLFW.swapBuffers window
  forever $ do
    GLFW.pollEvents
    draw drawables window

render :: Window ->  RenderHint -> Drawable -> IO ()
render window hint drawable  = do
  mesh <- createMesh drawable
  renderMesh hint mesh

renderMesh :: RenderHint -> Mesh -> IO ()
renderMesh hint@(RenderHint mode startIndex numVertices) mesh = withVertexArrayObject (vao mesh) $ do
  drawArrays mode (fromIntegral startIndex) (fromIntegral numVertices)
