module Renderable where

import qualified Graphics.Rendering.OpenGL as GL hiding (Matrix, Position)
import qualified Matrix as M
import Data.Matrix
import Animation
import Ease
import Transformation 

data Drawable = Drawable {
  vertices :: [GL.Vertex4 Float],
  colors :: [GL.Color4 Float],
  numberOfVertices :: Int
}
instance Show Drawable where
  show (Drawable vertices colors num) = "Vertices: " ++ (show vertices) ++ "\n" ++ "Colors: " ++ (show colors) ++ "\n" ++ "Number of Vertices: " ++ (show num)

data Renderable = Renderable {
  drawable:: Drawable,
  transformation :: Transformation,
  mvpMatrix ::  Matrix Float,
  animations :: [Animation]
}

makeDefaultRenderable :: Drawable -> Renderable
makeDefaultRenderable drawable = Renderable drawable defaultTransformation M.defaultMatrix [sampleAnimation RotationZ]
