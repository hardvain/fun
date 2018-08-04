module OpenGL.Window (
  createWindow, 
  closeWindow
) where

import qualified Graphics.UI.GLFW as GLFW
import System.Exit ( exitWith, ExitCode(..) )
import Graphics.Rendering.OpenGL as GL

initWindow :: IO ()
initWindow = do
  _ <- GLFW.init
  GLFW.defaultWindowHints
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 4
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 1
  GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
  GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True

createWindow :: Int -> Int -> String -> IO (GLFW.Window)
createWindow width height title = do
  initWindow
  Just win <- GLFW.createWindow width height title Nothing Nothing
  GLFW.makeContextCurrent (Just win)
  initCallbacks win
  GL.blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  GL.blend GL.$= GL.Enabled
  return win

initCallbacks :: GLFW.Window -> IO ()
initCallbacks win = do
  -- GLFW.setWindowSizeCallback win (Just resizeWindow)
  GLFW.setKeyCallback win (Just keyPressed)
  GLFW.setWindowCloseCallback win (Just shutdown)


-- resizeWindow :: GLFW.WindowSizeCallback
-- resizeWindow win w h =
--     do
--       GL.viewport   $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
--       GL.matrixMode $= GL.Projection
--       GL.loadIdentity
--       GL.ortho2D 0 (realToFrac w) (realToFrac h) 0


keyPressed :: GLFW.KeyCallback 
keyPressed win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown win
keyPressed _   _               _ _                     _ = return ()


shutdown :: GLFW.WindowCloseCallback
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return ()

closeWindow :: GLFW.Window -> IO ()
closeWindow win = do
    GLFW.destroyWindow win
    GLFW.terminate
  