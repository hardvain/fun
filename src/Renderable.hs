module Renderable where

import Graphics.Rendering.OpenGL as GL hiding (Matrix, Position)
import Matrix
import Data.Matrix

type MVPMatrix = Matrix Float

data Drawable = Drawable {
  vertices :: [Vertex4 Float],
  colors :: [Color4 Float],
  numberOfVertices :: Int
}


data Position = Position Float Float Float
data Rotation = Rotation Float Float Float
data Scale = Scale Float Float Float

data Transformation = Transformation Position Rotation Scale


data Renderable = Renderable {
  drawable:: Drawable,
  transformation :: Transformation,
  mvpMatrix ::  MVPMatrix
}


defaultPosition :: Position
defaultPosition = Position 0 0 0

defaultRotation :: Rotation
defaultRotation = Rotation 0 0 0

defaultScale :: Scale
defaultScale = Scale 1 1 1

defaultTransformation :: Transformation
defaultTransformation = Transformation defaultPosition defaultRotation defaultScale

makeDefaultRenderable :: Drawable -> Renderable
makeDefaultRenderable drawable = Renderable drawable defaultTransformation defaultMatrix
