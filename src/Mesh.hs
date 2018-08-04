module Mesh where

import SceneGraph
import Graphics.Rendering.OpenGL as GL hiding (Matrix, Position)
import Renderable
import Drawable
import Buffer
import System.IO.Unsafe

data Mesh = Mesh {
  positionBufferObject :: GL.BufferObject,
  colorBufferObject :: GL.BufferObject,
  vao :: GL.VertexArrayObject,
  renderable :: Renderable
}


data RenderHint = RenderHint {
  mode :: GL.PrimitiveMode,
  startIndex :: Int,
  numVertices :: Int
}

renderHint :: Mesh -> RenderHint
renderHint mesh = RenderHint GL.Triangles 0 (numberOfVertices . drawable . renderable $ mesh)

createMesh :: Renderable -> IO Mesh
createMesh renderable@(Renderable (Drawable positions colorsData _) _ _ ) =
  withNewVertexArrayObject $ \vao -> do
    positionBufferObject <- createAndDescribeBuffer positions 0 4
    colorBufferObject <- createAndDescribeBuffer colorsData 1 4
    return $ Mesh positionBufferObject colorBufferObject vao renderable


populateMeshes :: SceneGraph Renderable -> SceneGraph Mesh
populateMeshes (SceneGraph tree) =  SceneGraph $ fmap (unsafePerformIO . createMesh) tree

    