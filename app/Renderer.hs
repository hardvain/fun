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
import qualified Data.Time.Clock.POSIX as Time

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

draw :: [Drawable] -> Window -> [UniformData] -> Int -> Integer -> IO ()
draw drawables window uniforms frameNumber startTime = do
  GL.clearColor $= Color4 0 0 0 1
  currentTime <- timeInMillis
  let elapsedTime = currentTime - startTime
  GL.clear [ColorBuffer]
  let value = (fromIntegral (mod (fromIntegral elapsedTime) 1000)) * 0.001
  mapM_ (setUniform value) uniforms
  mapM_ (render window) drawables
  GLFW.swapBuffers window
  -- _ <- putStrLn ("Frames Elapsed: " ++ (show frameNumber) ++ ", MillisElapsed: " ++ (show elapsedTime))
  forever $ do
    GLFW.pollEvents
    draw drawables window uniforms (frameNumber + 1) startTime

render :: Window -> Drawable -> IO ()
render window  drawable@(_,_, numVertices)  = do
  let renderHint = RenderHint GL.Triangles 0 numVertices
  mesh <- createMesh drawable
  renderMesh renderHint mesh

renderMesh :: RenderHint -> Mesh -> IO ()
renderMesh hint@(RenderHint mode startIndex numVertices) mesh = withVertexArrayObject (vao mesh) $ do
  drawArrays mode (fromIntegral startIndex) (fromIntegral numVertices)
