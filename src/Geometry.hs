module Geometry where
  
import Graphics.Rendering.OpenGL (Vertex4(..), Color4(..), GLclampf(..))
import Utils 
import Color 

type Points    =  [Point]
type Point     =  (Float, Float, Float)
type Radius    =  Float
type Side      =  Float
type Divisions =  Int

data Geometry = Circle   Point   Radius Divisions
          | Square    Point   Side
          | Rect      Point   Point
          | Line      Point   Point  Float  -- | Ordered pair to store directionality
          | Triangle  Point   Point  Point
          | Quad     [Point]    -- | BL vertex TR vertex
          | Polygon  [Point]    -- | [Triangle] ? 
          | Polyline [Point]  Float
          | Curve    [Point]
          | Cube    Float
          deriving Show



vertex :: Point -> Vertex4 Float
vertex p = (\(k,l,m) -> Vertex4 k l m 1) p


geometry :: Geometry -> [Point]
geometry (Square   pos side)     =  square pos side
geometry (Circle   pos rad divs) =  circle pos rad divs
geometry (Rect     bl  tr)       =  rect   bl  tr        -- | bl := bottom left, tr := top right
geometry (Line     p1  p2  w)    =  line   p1  p2  w
geometry (Polyline ps  w)        =  polyline ps w
geometry (Triangle p1  p2 p3)    =  triangle p1 p2 p3


polyline :: [Point] -> Float -> [Point]
polyline ps w = concatMap (\(x,y,z) -> line x y w) $ triples (abbcca ps)


triangle :: Point -> Point -> Point -> [Point]
triangle p1 p2 p3 = [p1, p2, p3]


square :: Point -> Float -> [Point]
square (x,y,z) side = [p1, p2, p3,
                    p1, p3, p4]
    where          
        r = side/2 
        p1 = (x + r, y + r, z)
        p2 = (x - r, y + r, z)
        p3 = (x - r, y - r, z)
        p4 = (x + r, y - r, z)
        

abbcca :: [a] -> [a]
abbcca (x:xs) = [x] ++ (concat $ map (\(x,y) -> [x,y]) $ map (\x -> (x, x)) (init xs)) ++ [last xs]
        

circle :: Point -> Float -> Int -> [Point]
circle pos@(x,y,z) r divs =
    let
        divs'    = fromIntegral divs
        sines   = map ((y +).(r *).sin) [0.0, 2*pi/divs' .. 2*pi]
        cosines = map ((x +).(r *).cos) [0.0, 2*pi/divs' .. 2*pi]       
    in
        concat $ insertpos $ abbcca $ zip3 sines cosines (cycle [0])
            where
                  insertpos (x:y:[]) = [[pos,x,y]]
                  insertpos (x:y:xs) = [pos,x,y] : insertpos xs


rect :: Point -> Point -> [Point]
rect (x1,y1,_) (x2,y2,_) = [(x2,y2,0),(x1,y2,0),(x1,y1,0),
                        (x2,y2,0),(x1,y1,0),(x2,y1,0)]


line :: Point -> Point -> Float -> [Point]
line (x1,y1,_) (x2,y2,_) w = map (addVectors (x1,y1,0)) $ rotate3D' theta $ rect (0.0,-w/2,0) (len,w/2,0) -- rotation is wrong
      where 
            (x,y) = normalize $ ((x2-x1),(y2-y1))
            theta = signum y * acos x                               -- | angle in radians
            len   = sqrt((x2-x1)^2+ (y2-y1)^2)

      