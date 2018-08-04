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
  renderable :: Renderable,
  meshProgram :: P.Program
}

data RenderHint = RenderHint {
  primitiveMode :: GL.PrimitiveMode,
  startIndex :: Int,
  numVertices :: Int
}

createMesh :: Renderable -> IO Mesh
createMesh renderable@(Renderable (Drawable positions colorsData _) _ _ ) =
  withNewVertexArrayObject $ \vao -> do
    prog <- P.defaultProgram
    positionBufferObject <- createAndDescribeBuffer positions 0 4
    colorBufferObject <- createAndDescribeBuffer colorsData 1 4
    return $ Mesh positionBufferObject colorBufferObject vao renderable prog

populateMeshes :: SceneGraph Renderable -> SceneGraph Mesh
populateMeshes (SceneGraph tree) =  SceneGraph $ fmap (unsafePerformIO . createMesh) tree
