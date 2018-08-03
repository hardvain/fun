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
import System.IO.Unsafe

createMesh :: Renderable -> IO Mesh
createMesh renderable@(Renderable (Drawable positions colorsData _) _ _ ) =
  withNewVertexArrayObject $ \vao -> do
    positionBufferObject <- createAndDescribeBuffer positions 0 4
    colorBufferObject <- createAndDescribeBuffer colorsData 1 4
    return $ Mesh positionBufferObject colorBufferObject vao renderable

setUniform :: (Uniform a) => Float -> UniformData a ->  IO ()
setUniform time (UniformData name datum location) = 
  GL.uniform location GL.$= datum
  
setClearColor :: Color4 Float -> IO ()
setClearColor color = do
  GL.clearColor $= color
  GL.clear [ColorBuffer]
  return ()

draw :: (Uniform a) => SceneGraph Renderable -> Window -> [UniformData a] -> Int -> Integer -> IO ()
draw sceneGraph window uniforms frameNumber startTime  = do
  let meshedSceneGraph = populateMeshes sceneGraph
  drawLoop meshedSceneGraph window uniforms frameNumber startTime 

populateMeshes :: SceneGraph Renderable -> SceneGraph Mesh
populateMeshes (SceneGraph tree) =  SceneGraph (fmap (\r -> unsafePerformIO(createMesh r) ) tree)

drawLoop :: (Uniform a) => SceneGraph Mesh -> Window -> [UniformData a] -> Int -> Integer -> IO ()
drawLoop sceneGraph window uniforms frameNumber startTime = do
  setClearColor $ Color4 0 0 0 1
  mapM_ (setUniform 0) uniforms
  renderSceneGraph window sceneGraph
  GLFW.swapBuffers window
  forever $ do
    GLFW.pollEvents
    drawLoop sceneGraph window uniforms (frameNumber + 1) startTime

render :: Window -> Mesh -> IO ()
render window mesh = do
  let renderableObj = renderable mesh
  let renderHint = RenderHint GL.Triangles 0 (numberOfVertices . drawable $ renderableObj)
  renderMesh renderHint mesh

renderMesh :: RenderHint -> Mesh -> IO ()
renderMesh (RenderHint mode startIndex numVertices) mesh = withVertexArrayObject (vao mesh) $ do
  drawArrays mode (fromIntegral startIndex) (fromIntegral numVertices)

renderSceneGraph :: Window -> SceneGraph  Mesh -> IO ()
renderSceneGraph window (SceneGraph tree) = renderTree window tree

renderTree :: Window -> Tree  Mesh -> IO ()
renderTree window Empty = return ()
renderTree window (Node mesh trees) = do
  render window mesh
  mapM_ (renderTree window) trees
