module Renderable where

import qualified Matrix as M
import Data.Matrix
import Animation
import Ease
import Transformation 
import Drawable 

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
