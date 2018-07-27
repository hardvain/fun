module Library (
  createLibrary,
  Library(..)
) where

import Control.Monad (unless, when)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import System.Exit
import System.IO
import Shader

data Library = Library {
  vertexShader :: GL.Shader,
  fragmentShader :: GL.Shader,
  program :: GL.Program
}

createLibrary :: String -> String -> IO (Maybe Library)
createLibrary vertexSource fragmentSource = do
  glProgram <- GL.createProgram
  (Just vertexShader) <- addVertexShader vertexSource  
  _ <- GL.attachShader glProgram vertexShader
  (Just fragmentShader) <- addFragmentShader fragmentSource
  _ <- GL.attachShader glProgram fragmentShader
  GL.linkProgram glProgram
  linkStatus <- GL.linkStatus glProgram
  GL.validateProgram glProgram
  putStrLn (show linkStatus)
  if (linkStatus )
    then 
      (return $ Just $ Library vertexShader fragmentShader glProgram) 
    else
      return Nothing
  
