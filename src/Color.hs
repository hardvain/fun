module Color where

import Data.Bits

data Color = Red
              | Green
              | Blue
              | White
              | Black
              | RGB    Float Float Float 
              | RGBA   Float Float Float Float
              | Default deriving Show

instance Eq Color where
  Red          == Red   = True
  Green        == Green = True
  Blue         == Blue  = True
  White        == White = True
  Black        == Black = True
  RGB _ _ _    == RGB _ _ _    = True
  RGBA _ _ _ _ == RGBA _ _ _ _ = True
  Default      == Default      = True
  _ == _ = False

hex :: Integer -> Color
hex v = RGB r g b
  where
    r = (fromIntegral $ shiftR (v .&. 0xFF0000) 16) / 255.0
    g = (fromIntegral $ shiftR (v .&. 0xFF00) 8) / 255.0
    b = (fromIntegral $ (v .&. 0xFF)) / 255.0