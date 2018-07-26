module Main where

import Control.Monad (unless, when)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import System.Exit
import System.IO

-- tiny utility functions, in the same spirit as 'maybe' or 'either'
-- makes the code a wee bit easier to read


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
      mw <- GLFW.createWindow 1920 1280 "Simple example, haskell style" Nothing Nothing
      maybe' mw (GLFW.terminate >> exitFailure) $ \window -> do
          GLFW.makeContextCurrent mw
          GLFW.setKeyCallback window (Just keyCallback)
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


newtype ShaderId = ShaderId Int
data ShaderType = Vertex | Fragment
data Shader = 
  VertexShader ShaderId
  | FragmentShader ShaderId

data Program = Program {
  vertexShader :: Shader,
  fragmentShader :: Shader
}

createShader :: String -> GL.ShaderType -> IO()
createShader shaderSource shaderType = do
  shaderId <- GL.createShader shaderType
  GL.shaderSourceBS shaderId GL.$= vsSource
  GL.compileShader shaderId


vsSource :: BS.ByteString
vsSource = BS.intercalate (BC.pack "\n")
            [
            (BC.pack "attribute vec2 coord2d; ")
            , (BC.pack "")
            , (BC.pack "void main(void) { ")
            , (BC.pack " gl_Position = vec4(coord2d, 0.0, 1.0); ")
            , (BC.pack "}")
            ]