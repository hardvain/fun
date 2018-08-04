module AST where

import Ease
import Graphics.Rendering.OpenGL as GL hiding (Matrix, Position)


type Points    =  [Point]
type Point     =  (Float, Float)
type Radius    =  Float
type Side      =  Float
type Divisions =  Int
