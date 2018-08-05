module Transformation where


data Position = Position Float Float Float deriving Show
data Rotation = Rotation Float Float Float deriving Show 
data Scale = Scale Float Float Float deriving Show

instance Semigroup Position where
  (Position x1 y1 z1) <> (Position x2 y2 z2) = Position (x1+x2) (y1+y2) (z1+z2)

instance Monoid Position where
  mempty = defaultPosition

instance Semigroup Rotation where
  (Rotation x1 y1 z1) <> (Rotation x2 y2 z2) = Rotation (x1+x2) (y1+y2) (z1+z2)

instance Monoid Rotation where
  mempty = defaultRotation
  
-- TODO: Fix addition of components in scale
instance Semigroup Scale where
  (Scale x1 y1 z1) <> (Scale x2 y2 z2) = Scale (x1+x2) (y1+y2) (z1+z2)

instance Monoid Scale where
  mempty = defaultScale  

data Transformation = Transformation Position Rotation Scale deriving Show

instance Semigroup Transformation where
  (Transformation p1 r1 s1) <> (Transformation p2 r2 s2) = Transformation (mappend p1 p2) (mappend r1 r2) (mappend s1 s2) 

instance Monoid Transformation where
  mempty = defaultTransformation
  

defaultPosition :: Position
defaultPosition = Position 0 0 0

defaultRotation :: Rotation
defaultRotation = Rotation 0 0 0

defaultScale :: Scale
defaultScale = Scale 1 1 1

defaultTransformation :: Transformation
defaultTransformation = Transformation defaultPosition defaultRotation defaultScale