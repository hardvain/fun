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

projectionMatrix :: Matrix Float
projectionMatrix = projection 45 1 0.01 100

translate :: Float -> Float -> Float -> Matrix Float
translate x y z = fromList 4 4 matrix
  where 
    matrix = [1, 0, 0, 0,
              0, 1, 0, 0,
              0, 0, 1, 0,
              x, y, z, 1]

scale :: Float -> Float -> Float -> Matrix Float
scale x y z = fromList 4 4 matrix
  where 
    matrix = [x, 0, 0, 0,
              0, y, 0, 0,
              0, 0, z, 0,
              0, 0, 0, 1]

rotate :: Float -> Float -> Float -> Float -> Matrix Float
rotate angle x y z = fromList 4 4 matrix
  where
    c = cos angle
    s = sin angle
    mc = (1 - c)
    r1c1 = x * x * mc + c
    r2c1 = x * y * mc + z * s
    r3c1 = x * z * mc - y * s
    r4c1 = 0
    r1c2 =  y * x * mc - z * s
    r2c2 = y * y * mc + c
    r3c2 = y * z * mc + x * s
    r4c2 = 0
    r1c3 = z * x * mc + y * s
    r2c3 = z * y * mc - x * s
    r3c3 = z * z * mc + c
    r4c3 = 0
    r1c4 = 0
    r2c4 = 0
    r3c4 = 0
    r4c4 = 1
    matrix = [r1c1, r2c2, r3c1, r4c1,
              r1c2, r2c2, r3c2, r4c2,
              r1c3, r2c3, r3c3, r4c3,
              r1c4, r2c4, r3c4, r4c4]

              