module Matrix where

import Data.Matrix


defaultMatrix :: Matrix Float
defaultMatrix = identity 4 

projection :: Float -> Float -> Float -> Float -> Matrix Float
projection degrees aspectRatio nearZ farZ = fromList 4 4 matrix
  where
    fov = degrees * (pi / 180)
    y = 1 / tan (fov * 0.5)
    x = y / aspectRatio
    z = farZ / (nearZ - farZ)
    w = (z * nearZ)
    matrix = [x, 0, 0, 0,
              0, y, 0, 0,
              0, 0, z, -1,
              0, 0, 0, w]