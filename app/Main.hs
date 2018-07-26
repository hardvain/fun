module Main where

  import Control.Monad (unless, when)
  import Graphics.Rendering.OpenGL
  import qualified Graphics.UI.GLFW as G
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
  errorCallback :: G.ErrorCallback
  errorCallback err description = hPutStrLn stderr description
  
  keyCallback :: G.KeyCallback
  keyCallback window key scancode action mods = when (key == G.Key'Escape && action == G.KeyState'Pressed) $
    G.setWindowShouldClose window True
  
  main :: IO ()
  main = do
    G.setErrorCallback (Just errorCallback)
    successfulInit <- G.init
    -- if init failed, we exit the program
    bool successfulInit exitFailure $ do
        mw <- G.createWindow 1920 1280 "Simple example, haskell style" Nothing Nothing
        maybe' mw (G.terminate >> exitFailure) $ \window -> do
            G.makeContextCurrent mw
            G.setKeyCallback window (Just keyCallback)
            mainLoop window
            G.destroyWindow window
            G.terminate
            exitSuccess
            
  mainLoop :: G.Window -> IO ()
  mainLoop w = unless' (G.windowShouldClose w) $ do
      clearColor $= Color4 0 0 0.4 0
      clear [ColorBuffer]
      G.swapBuffers w
      G.pollEvents
      mainLoop w