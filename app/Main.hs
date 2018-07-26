module Main where

import Control.Monad (unless, when)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import System.Exit
import System.IO
import Library
-- tiny utility functions, in the same spirit as 'maybe' or 'either'
-- makes the code a wee bit easier to read

vertexShaderPath = "/Users/aravindhs/Aravindh/projects/haskell/fun/shaders/vertex.shader" 
fragmentShaderPath = "/Users/aravindhs/Aravindh/projects/haskell/fun/shaders/fragment.shader" 

bool :: Bool -> a -> a -> a
bool b falseRes trueRes = if b then trueRes else falseRes

unless' :: Monad m => m Bool -> m () -> m ()
unless' action falseAction = do
    b <- action
    unless b falseAction

maybe' :: Maybe a -> b -> (a -> b) -> b
maybe' m nothingRes f = case m of
    Nothing -> nothingRes
    Just x  -> f x
    
-- type ErrorCallback = Error -> String -> IO ()
errorCallback :: GLFW.ErrorCallback
errorCallback err description = hPutStrLn stderr description

keyCallback :: GLFW.KeyCallback
keyCallback window key scancode action mods = when (key == GLFW.Key'Escape && action == GLFW.KeyState'Pressed) $
  GLFW.setWindowShouldClose window True

main :: IO ()
main = do
  GLFW.setErrorCallback (Just errorCallback)
  successfulInit <- GLFW.init
  -- if init failed, we exit the program
  bool successfulInit exitFailure $ do
      GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 4
      GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 1
      GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
      GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
      mw <- GLFW.createWindow 1920 1280 "Simple example, haskell style" Nothing Nothing
      maybe' mw (GLFW.terminate >> exitFailure) $ \window -> do
          GLFW.makeContextCurrent mw
          GLFW.setKeyCallback window (Just keyCallback)
          lib <- createLibrary vertexShaderPath fragmentShaderPath
          mainLoop window
          GLFW.destroyWindow window
          GLFW.terminate
          exitSuccess
          
mainLoop :: GLFW.Window -> IO ()
mainLoop w = unless' (GLFW.windowShouldClose w) $ do
    GL.clearColor GL.$= GL.Color4 0 0.5 0.5 1
    GL.clear [GL.ColorBuffer]
    GLFW.swapBuffers w
    GLFW.pollEvents
    mainLoop w
