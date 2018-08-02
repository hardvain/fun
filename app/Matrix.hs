module Matrix where


data Matrix a =
  Matrix !a !a !a !a
           !a !a !a !a
           !a !a !a !a
           !a !a !a !a
             deriving Eq

defaultMatrix :: [Float]
defaultMatrix = [ 1.0, 0.0, 0.0, 0.0
  , 0.0, 1.0, 0.0, 0.0
  , 0.0, 0.0, 1.0, 0.0
  , 0.0, 0.0, 0.0, 0.5 ]  