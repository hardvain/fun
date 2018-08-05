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

sampleAnimation :: AnimationTarget -> Animation 
sampleAnimation target = Animation target (0) (6.28)  BounceInOut 0 5000

modelMatrix :: Transformation -> Matrix Float -> Matrix Float
modelMatrix (Transformation (Position x y z) r@(Rotation a b c) (Scale l m n)) currentMatrix = result
  where
    translateMatrix = M.translate x y z
    scaleMatrix = M.scale l m n
    result = rotate r (multStd (multStd translateMatrix currentMatrix) scaleMatrix) 

rotate :: Rotation -> Matrix Float -> Matrix Float
rotate (Rotation x y z) matrix = result
  where
    rotatedX = if x > 0 then multStd matrix (M.rotate x 1 0 0) else matrix
    rotatedY = if y > 0 then multStd rotatedX (M.rotate y 0 1 0) else rotatedX
    rotatedZ = if z > 0 then multStd rotatedY (M.rotate z 0 0 0) else rotatedY
    result = rotatedZ