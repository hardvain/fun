module Renderable where

import Drawable
import Graphics.Rendering.OpenGL as GL hiding (Matrix, Position)
import Data.Matrix

type MVPMatrix = Matrix Float

data Position = Position Float Float Float
data Rotation = Rotation Float Float Float
data Scale = Scale Float Float Float

data Transformation = Transformation Position Rotation Scale


data Renderable = Renderable {
  drawable:: Drawable,
  transformation :: Transformation,
  mvpMatrix ::  MVPMatrix
}