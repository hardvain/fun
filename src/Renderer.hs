{-# LANGUAGE FlexibleContexts #-}
module Renderer where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Control.Monad
import Buffer 
import qualified Program as P
import Shape 
import Data.Ratio
import Ease 
import Time
import AST
import Matrix as M
import qualified Data.Time.Clock.POSIX as Time
import System.IO.Unsafe
import Data.Matrix



createMesh :: Renderable -> IO Mesh
createMesh renderable@(Renderable (Drawable positions colorsData _) _ _ ) =
  withNewVertexArrayObject $ \vao -> do
    positionBufferObject <- createAndDescribeBuffer positions 0 4
    colorBufferObject <- createAndDescribeBuffer colorsData 1 4
    return $ Mesh positionBufferObject colorBufferObject vao renderable


populateMeshes :: SceneGraph Renderable -> SceneGraph Mesh
populateMeshes (SceneGraph tree) =  SceneGraph $ fmap (unsafePerformIO . createMesh) tree

    
setUniform1 :: Renderable ->  IO ()
setUniform1 renderableObj = do
  let matrix = mvpMatrix renderableObj
  prog <- P.defaultProgram
  transformLocation <- GL.uniformLocation (P.glProgram prog) "transform"
  datum <- GL.newMatrix GL.ColumnMajor (toList matrix) :: IO (GL.GLmatrix GL.GLfloat)
  GL.uniform transformLocation GL.$= datum
  
setClearColor :: Color4 Float -> IO ()
setClearColor color = do
  GL.clearColor $= color
  GL.clear [ColorBuffer]
  return ()

setMVPMatrix :: Mesh -> IO ()
setMVPMatrix mesh = P.setMVPMatrix (mvpMatrix . renderable $ mesh)
  
draw ::  SceneGraph Renderable -> Window ->  Int -> Integer -> IO ()
draw sceneGraph  = drawLoop (populateMeshes sceneGraph)

drawLoop ::  SceneGraph Mesh -> Window -> Int -> Integer -> IO ()
drawLoop sceneGraph window frameNumber startTime = do
  setClearColor $ Color4 0 0 0 1
  renderSceneGraph sceneGraph
  GLFW.swapBuffers window
  forever $ do
    GLFW.pollEvents
    drawLoop sceneGraph window (frameNumber + 1) startTime

renderHint :: Mesh -> RenderHint
renderHint mesh = RenderHint GL.Triangles 0 (numberOfVertices . drawable . renderable $ mesh)
    
render :: Mesh -> IO ()
render mesh = do
  _ <- setMVPMatrix mesh
  let renderableObj = renderable mesh
  withVertexArrayObject (vao mesh) $ do
    let (RenderHint mode startIndex numVertices) = renderHint mesh
    drawArrays mode (fromIntegral startIndex) (fromIntegral numVertices)


renderSceneGraph :: SceneGraph  Mesh -> IO ()
renderSceneGraph (SceneGraph tree) = renderTree tree

renderTree :: Tree Mesh -> IO ()
renderTree Empty = return ()
renderTree (Node mesh trees) = do
  render mesh
  mapM_ (renderTree) trees
