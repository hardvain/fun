module Mesh where

import qualified Graphics.Rendering.OpenGL as GL hiding (Matrix, Position)

import OpenGL.Buffer 
import System.IO.Unsafe
import qualified OpenGL.Program as P
import Renderable as R
import Data.Matrix 
import Matrix
data Mesh = Mesh {
  positionBufferObject :: GL.BufferObject,
  colorBufferObject :: GL.BufferObject,
  vao :: GL.VertexArrayObject,
  renderable :: Renderable
}

data RenderPipelineState = RenderPipelineState {
  mesh :: Mesh,
  program :: P.Program,
  modelMatrix :: Matrix Float
}

data RenderHint = RenderHint {
  primitiveMode :: GL.PrimitiveMode,
  startIndex :: Int,
  numVertices :: Int
}

initializePipelineState ::  Renderable -> IO RenderPipelineState
initializePipelineState renderable@(Renderable (Drawable positions colorsData _) _ _ _) =
  withNewVertexArrayObject $ \vao -> do
    prog <- P.defaultProgram
    positionBufferObject <- createAndDescribeBuffer positions 0 4
    colorBufferObject <- createAndDescribeBuffer colorsData 1 4
    let mesh = Mesh positionBufferObject colorBufferObject vao renderable
    return $ RenderPipelineState mesh prog (mvpMatrix renderable)

