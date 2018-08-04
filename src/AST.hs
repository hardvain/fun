module AST where

import Data.Matrix
import Ease
import Graphics.Rendering.OpenGL as GL hiding (Matrix, Position)


type Points    =  [Point]
type Point     =  (Float, Float)
type Radius    =  Float
type Side      =  Float
type Divisions =  Int
type MVPMatrix = Matrix Float
data ProgramType = DefaultProg

data Renderable = Renderable {
  drawable:: Drawable,
  transformation :: Transformation,
  mvpMatrix ::  MVPMatrix
}

data Drawable = Drawable {
  vertices :: [Vertex4 Float],
  colors :: [Color4 Float],
  numberOfVertices :: Int
}

data Position = Position Float Float Float
data Rotation = Rotation Float Float Float
data Scale = Scale Float Float Float

data Transformation = Transformation Position Rotation Scale
