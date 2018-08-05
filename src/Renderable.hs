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
instance Show Drawable where
  show (Drawable vertices colors num) = "Vertices: " ++ (show vertices) ++ "\n" ++ "Colors: " ++ (show colors) ++ "\n" ++ "Number of Vertices: " ++ (show num)

data Position = Position Float Float Float deriving Show
instance Semigroup Position where
  (Position x1 y1 z1) <> (Position x2 y2 z2) = Position (x1+x2) (y1+y2) (z1+z2)

instance Monoid Position where
  mempty = defaultPosition

instance Semigroup Rotation where
  (Rotation x1 y1 z1) <> (Rotation x2 y2 z2) = Rotation (x1+x2) (y1+y2) (z1+z2)

instance Monoid Rotation where
  mempty = defaultRotation

instance Semigroup Scale where
  (Scale x1 y1 z1) <> (Scale x2 y2 z2) = Scale (x1+x2) (y1+y2) (z1+z2)

instance Monoid Scale where
  mempty = defaultScale  

data Rotation = Rotation Float Float Float deriving Show 
data Scale = Scale Float Float Float deriving Show

data Transformation = Transformation Position Rotation Scale deriving Show

instance Semigroup Transformation where
  (Transformation p1 r1 s1) <> (Transformation p2 r2 s2) = Transformation (mappend p1 p2) (mappend r1 r2) (mappend s1 s2) 

instance Monoid Transformation where
  mempty = defaultTransformation

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
makeDefaultRenderable drawable = Renderable drawable defaultTransformation M.defaultMatrix [sampleAnimation RotationX]

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