module Main where

import Graphics.UI.GLUT
import Lib

main :: IO ()
main = do
  (_programName, _args) <- getArgsAndInitialize
  _window <- createWindow "Animax"
  displayCallback $= display
  mainLoop

display :: DisplayCallback
display = do
  clear [ColorBuffer]
  flush
