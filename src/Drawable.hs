module Drawable where

import qualified Graphics.Rendering.OpenGL as GL hiding (Matrix, Position)
import Color
import Geometry

data Drawable = Drawable {
  vertices :: [GL.Vertex4 Float],
  colors :: [GL.Color4 Float],
  numberOfVertices :: Int
}

makeDrawable :: Geometry -> Color -> Drawable
makeDrawable g clr = Drawable vertices colors (length vertices)
    where
      vertices  = map vertex $ geometry g
      color     = getColor clr
      colors    = map (const color) $ vertices
