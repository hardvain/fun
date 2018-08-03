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
import Program 
import Data.Matrix



createMesh :: Renderable -> IO Mesh
createMesh renderable@(Renderable (Drawable positions colorsData _) _ _ ) =
  withNewVertexArrayObject $ \vao -> do
    positionBufferObject <- createAndDescribeBuffer positions 0 4
    colorBufferObject <- createAndDescribeBuffer colorsData 1 4
    return $ Mesh positionBufferObject colorBufferObject vao renderable


populateMeshes :: SceneGraph Renderable -> SceneGraph Mesh
populateMeshes (SceneGraph tree) =  SceneGraph $ fmap (unsafePerformIO . createMesh) tree

    
setUniform :: (Uniform a) => UniformData a ->  IO ()
setUniform (UniformData name datum location) = 
  GL.uniform location GL.$= datum
  
setClearColor :: Color4 Float -> IO ()
setClearColor color = do
  GL.clearColor $= color
  GL.clear [ColorBuffer]
  return ()

draw ::  SceneGraph Renderable -> Window ->  Int -> Integer -> IO ()
draw sceneGraph window frameNumber startTime  = do
  let meshedSceneGraph = populateMeshes sceneGraph
  -- mapM_ (setUniform 0) uniforms
  drawLoop meshedSceneGraph window frameNumber startTime 

drawLoop ::  SceneGraph Mesh -> Window -> Int -> Integer -> IO ()
drawLoop sceneGraph window frameNumber startTime = do
  setClearColor $ Color4 0 0 0 1
  renderSceneGraph window sceneGraph
  GLFW.swapBuffers window
  forever $ do
    GLFW.pollEvents
    drawLoop sceneGraph window (frameNumber + 1) startTime

render :: Window -> Mesh -> IO ()
render window mesh = do
  let renderableObj = renderable mesh
  let matrix = mvpMatrix renderableObj
  prog <- defaultProgram
  transformLocation <- GL.uniformLocation (glProgram prog) "transform"
  transform <- GL.newMatrix GL.ColumnMajor (toList matrix) :: IO (GL.GLmatrix GL.GLfloat)
  setUniform $ UniformData "transform" transform transformLocation
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
