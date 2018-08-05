module Renderable where

import qualified Graphics.Rendering.OpenGL as GL hiding (Matrix, Position)
import qualified Matrix as M
import Data.Matrix
import Animation
import Ease

data Drawable = Drawable {
  vertices :: [GL.Vertex4 Float],
  colors :: [GL.Color4 Float],
  numberOfVertices :: Int
}


data Position = Position Float Float Float
data Rotation = Rotation Float Float Float
data Scale = Scale Float Float Float

data Transformation = Transformation Position Rotation Scale


data Renderable = Renderable {
  drawable:: Drawable,
  transformation :: Transformation,
  mvpMatrix ::  Matrix Float,
  animations :: [Animation]
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
makeDefaultRenderable drawable = Renderable drawable defaultTransformation (M.translate 0.2 (-0.4) 0) [sampleAnimation PositionX]

sampleAnimation :: AnimationTarget -> Animation 
sampleAnimation target = Animation target (-1) (1)  BounceInOut 0 5000

modelMatrix :: Transformation -> Matrix Float -> Matrix Float
modelMatrix (Transformation (Position x y z) (Rotation a b c) (Scale l m n))currentMatrix = result
  where
    translateMatrix = M.translate x y z
    scaleMatrix = M.scale l m n
    result = multStd (multStd currentMatrix translateMatrix) scaleMatrix
