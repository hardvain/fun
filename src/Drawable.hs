module Drawable where

import Graphics.Rendering.OpenGL as GL hiding (Matrix, Position)

data Drawable = Drawable {
  vertices :: [Vertex4 Float],
  colors :: [Color4 Float],
  numberOfVertices :: Int
}