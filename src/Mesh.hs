module Mesh where

import qualified Graphics.Rendering.OpenGL as GL hiding (Matrix, Position)
import AST
import Buffer 
import System.IO.Unsafe
import qualified Program as P

data Mesh = Mesh {
  positionBufferObject :: GL.BufferObject,
  colorBufferObject :: GL.BufferObject,
  vao :: GL.VertexArrayObject,
  renderable :: Renderable
}

data RenderPipelineState = RenderPipelineState {
  mesh :: Mesh,
  program :: P.Program
}

data RenderHint = RenderHint {
  primitiveMode :: GL.PrimitiveMode,
  startIndex :: Int,
  numVertices :: Int
}

createMesh :: Renderable -> IO RenderPipelineState
createMesh renderable@(Renderable (Drawable positions colorsData _) _ _ ) =
  withNewVertexArrayObject $ \vao -> do
    prog <- P.defaultProgram
    positionBufferObject <- createAndDescribeBuffer positions 0 4
    colorBufferObject <- createAndDescribeBuffer colorsData 1 4
    let mesh = Mesh positionBufferObject colorBufferObject vao renderable
    return $ RenderPipelineState mesh prog

populateMeshes :: SceneGraph Renderable -> SceneGraph RenderPipelineState
populateMeshes (SceneGraph tree) =  SceneGraph $ fmap (unsafePerformIO . createMesh) tree
