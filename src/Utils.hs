module Utils where

-- | converts degrees to radians
toRadians :: Float -> Float
toRadians x = x*pi/180


-- | converts radians to degrees
fromRadians :: Float -> Float
fromRadians x = x/pi*180

rotate3D' :: Float -> [(Float, Float, Float)] -> [(Float, Float, Float)]
rotate3D' a = map (rotate3D a)

rotate3D :: Float -> (Float, Float, Float) -> (Float, Float, Float)
rotate3D theta (x,y,z) = (x',y',0)
  where
    x' = x * cos theta - y * sin theta
    y' = x * sin theta + y * cos theta


normalize :: (Float, Float) -> (Float, Float)
normalize v@(x,y) = (x*len', y*len')
  where
    len' = 1.0/len v

len :: (Float, Float) -> Float
len (x,y) = sqrt(x*x+y*y)


addVectors :: (Float, Float, Float) -> (Float, Float, Float) -> (Float, Float, Float)
addVectors (x1,y1,z1) (x2,y2,z2) = (x1+x2, y1+y2, z1+z1)


-- | Group list into indevidual pairs: [1,2,3,4] => [(1,2),(3,4)]. 
--   Works only with even number of elements
triples :: [t] -> [(t, t, t)]
triples [] = []
triples [x] = error "Non-triple list for pair function with one value"
triples [x,y] = error "Non-triple list for pair function with two values"
triples (x:y:z:xs) = (x,y, z):triples xs

-- | Undo pairs function
fromTriples :: [(a, a, a)] -> [a]
fromTriples [] = []
fromTriples ((x,y, z):xs) = x:y:z:fromTriples xs

