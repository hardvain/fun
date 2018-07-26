module Shader (
  ShaderType(..),
  Shader(..),
  addVertexShader,
  addFragmentShader
) where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB

newtype ShaderId = ShaderId GL.GLuint

data ShaderType = Vertex | Fragment deriving Show
data Shader = 
  VertexShader ShaderId
  | FragmentShader ShaderId


addVertexShader :: String -> IO (Maybe GL.Shader)
addVertexShader vertexSource = addShader vertexSource Vertex

addFragmentShader :: String -> IO (Maybe GL.Shader)
addFragmentShader fragmentSource = addShader fragmentSource Fragment

createShader :: ShaderType -> IO GL.Shader
createShader = GL.createShader . toGLShader

setShaderSource :: GL.Shader -> BS.ByteString -> IO ()
setShaderSource shader shaderSource = (GL.shaderSourceBS shader) GL.$= shaderSource

compileShader :: GL.Shader -> IO()
compileShader = GL.compileShader

getCompileStatus :: GL.Shader -> IO Bool
getCompileStatus = GL.compileStatus

getShaderLog :: GL.Shader -> IO String
getShaderLog = GL.shaderInfoLog


addShader :: String -> ShaderType -> IO (Maybe GL.Shader)
addShader shaderPath shaderType = do
  shader <- createShader shaderType
  shaderSource <- loadShaderSource shaderPath
  setShaderSource shader shaderSource
  compileShader shader
  status <- getCompileStatus shader
  log <- getShaderLog shader
  if status then (return $ Just shader) else return Nothing


loadShaderSource :: String -> IO BS.ByteString
loadShaderSource = BS.readFile

toGLShader :: ShaderType -> GL.ShaderType
toGLShader Vertex = GL.VertexShader
toGLShader Fragment = GL.FragmentShader