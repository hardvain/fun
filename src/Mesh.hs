module Mesh where

import qualified Graphics.Rendering.OpenGL as GL hiding (Matrix, Position)

import OpenGL.Buffer 
import System.IO.Unsafe
import qualified OpenGL.Program as P
import Renderable as R
import Data.Matrix 
import Matrix
import Drawable
import Animation

data Mesh = Mesh {
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
    let mesh = Mesh vao renderable
    return $ RenderPipelineState mesh prog (mvpMatrix renderable)

numVerticesToDraw :: Mesh -> Int
numVerticesToDraw = numberOfVertices . drawable . renderable

meshAnimations :: Mesh -> [Animation]
meshAnimations = animations . renderable

meshModelMatrix :: Mesh -> Matrix Float
meshModelMatrix = mvpMatrix . renderable

