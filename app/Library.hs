module Library (
  createLibrary,
  Library
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
  fragmentShader :: GL.Shader
}

createLibrary :: String -> String -> IO Library
createLibrary vertexSource fragmentSource = do
  glProgram <- GL.createProgram
  vertexShader <- addVertexShader vertexSource  
  _ <- GL.attachShader glProgram vertexShader
  fragmentShader <- addFragmentShader fragmentSource
  _ <- GL.attachShader glProgram fragmentShader
  return (Library vertexShader fragmentShader)
