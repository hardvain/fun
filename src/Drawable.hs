module Drawable where

import qualified Graphics.Rendering.OpenGL as GL hiding (Matrix, Position)
import Color
import Geometry

data Drawable = Drawable {
  vertices :: [GL.Vertex4 Float],
  colors :: [GL.Color4 Float],
  numberOfVertices :: Int
}

toDrawable :: Color -> Geometry -> Drawable
toDrawable clr x = Drawable vertices colors (length vertices)
    where
      vertices  = map vertex $ geometry x
      color     = getColor clr
      colors    = map (const color) $ vertices
